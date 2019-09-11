{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception (fromException, catch, AsyncException, SomeException)
import Control.Monad (forM, void, when)
import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Options.Applicative (execParser)
import System.Environment (getEnvironment)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import qualified Prometheus
import qualified Prometheus.Metric.GHC
import qualified System.Posix.Signals as Signals
import qualified System.Log.Raven.Types as Sentry

import Config (Config (..), configInfo)
import Core (Core (..))
import Logger (Logger, postLog)

import qualified Core
import qualified HttpServer
import qualified Server
import qualified WebsocketServer
import qualified Logger
import qualified Metrics
import qualified MetricsServer
import qualified SentryLogging

-- Install SIGTERM and SIGINT handlers to do a graceful exit.
installHandlers :: Core -> IO ()
installHandlers core =
  let
    handle = do
      postLog (coreLogger core) "\nTermination sequence initiated ..."
      Core.postQuit core
    handler = Signals.CatchOnce handle
    blockSignals = Nothing
    installHandler signal = Signals.installHandler signal handler blockSignals
  in do
    void $ installHandler Signals.sigTERM
    void $ installHandler Signals.sigINT


main :: IO ()
main = do
  crashLogger <- SentryLogging.getCrashLogger
  SentryLogging.runWithCrashLogger "Control loop error" crashLogger $ do
    -- make sure output is flushed regularly
    hSetBuffering stdout LineBuffering

    env <- getEnvironment
    config <- execParser (configInfo env)

    -- start logging as early as possible
    logger <- Logger.newLogger (fromIntegral $ configQueueCapacity config)
    loggerThread <- Async.async $ Logger.processLogRecords logger

    -- setup metrics if enabled
    icepeakMetrics <- forM (configMetricsEndpoint config) $ const $ do
      void $ Prometheus.register Prometheus.Metric.GHC.ghcMetrics
      Metrics.createAndRegisterIcepeakMetrics

    eitherCore <- Core.newCore config logger icepeakMetrics
    either putStrLn (runCore crashLogger) eitherCore

    -- only stop logging when everything else has stopped
    Logger.postStop logger
    Async.wait loggerThread

runCore :: Maybe Sentry.SentryService -> Core -> IO ()
runCore mSentryService core = do
  let config = coreConfig core
  let logger = coreLogger core
  httpServer <- HttpServer.new mSentryService core
  let wsServer = WebsocketServer.acceptConnection core

  -- start threads
  commandLoopThread <- Async.async $ catchRunCoreResult CommandLoopException $ Core.runCommandLoop core
  webSocketThread <- Async.async $ catchRunCoreResult WebSocketsException $ WebsocketServer.processUpdates core
  httpThread <- Async.async $ catchRunCoreResult HttpException $ Server.runServer logger wsServer httpServer (configPort config)
  syncThread <- Async.async $ Core.runSyncTimer core
  metricsThread <- Async.async
    $ forM_ (configMetricsEndpoint config) (MetricsServer.runMetricsServer logger)

  installHandlers core
  logAuthSettings config logger
  logQueueSettings config logger
  logSyncSettings config logger
  postLog logger "System online. ** robot sounds **"

  -- Everything should stop when any of these stops
  (_, runCoreResult) <- Async.waitAny [commandLoopThread, webSocketThread, httpThread]
  logRunCoreResult mSentryService logger runCoreResult

  -- kill all threads when one of the main threads ended
  Core.postQuit core -- should stop commandLoopThread
  Async.cancel webSocketThread
  Async.cancel httpThread
  Async.cancel metricsThread
  Async.cancel syncThread
  void $ Async.wait commandLoopThread

-- | Data type to hold results for the async that finishes first
data RunCoreResult
  = CommandLoopException SomeException
  | WebSocketsException SomeException
  | HttpException SomeException
  | ThreadOk

-- | If a threads fails, catch the error and tag it so we know how to log it
catchRunCoreResult :: (SomeException -> RunCoreResult) -> IO () -> IO RunCoreResult
catchRunCoreResult tag action = catch (action >> pure ThreadOk) $ \exc -> case fromException exc of
    Just (_ :: AsyncException) -> pure ThreadOk
    _ -> pure (tag exc) -- we only worry about non-async exceptions

logRunCoreResult :: Maybe Sentry.SentryService -> Logger -> RunCoreResult -> IO ()
logRunCoreResult mSentryService logger rcr = do
    case rcr of
      CommandLoopException exc -> handleLog "core" exc
      WebSocketsException exc -> handleLog "web sockets server" exc
      HttpException exc -> handleLog "http server" exc
      ThreadOk -> pure ()
  where
    handleLog name exc
      | Just (_ :: AsyncException) <- fromException exc = pure ()
      | otherwise = do
            Logger.postLog logger $ name <> " stopped with an exception: " <> Text.pack (show exc)
            SentryLogging.logException "Log Run Core Error" mSentryService exc

logAuthSettings :: Config -> Logger -> IO ()
logAuthSettings cfg logger
  | configEnableJwtAuth cfg = case configJwtSecret cfg of
      Just _ -> postLog logger "JWT authorization enabled and secret provided, tokens will be verified."
      Nothing -> postLog logger "JWT authorization enabled but no secret provided, tokens will NOT be verified."
  | otherwise = case configJwtSecret cfg of
      Just _ -> postLog logger "WARNING a JWT secret has been provided, but JWT authorization is disabled."
      Nothing -> postLog logger "JWT authorization disabled."

logQueueSettings :: Config -> Logger -> IO ()
logQueueSettings cfg logger =
  postLog logger ("Queue capacity is set to " <> Text.pack (show (configQueueCapacity cfg)) <> ".")

logSyncSettings :: Config -> Logger -> IO ()
logSyncSettings cfg logger = case configSyncIntervalMicroSeconds cfg of
  Nothing -> do
    postLog logger "Sync: Persisting after every modification"
    when (configEnableJournaling cfg) $ do
      postLog logger "Journaling has no effect when periodic syncing is disabled"
  Just musecs -> do
    postLog logger ("Sync: every " <> Text.pack (show musecs) <> " microseconds.")
    when (configEnableJournaling cfg) $ do
      postLog logger "Journaling enabled"
