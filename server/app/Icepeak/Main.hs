{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.Async (Async)
import Control.Exception (fromException, AsyncException)
import Control.Monad (forM, void, when)
import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Options.Applicative (execParser)
import System.Environment (getEnvironment)

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import qualified Prometheus
import qualified Prometheus.Metric.GHC
import qualified System.Posix.Signals as Signals

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

-- Install SIGTERM and SIGINT handlers to do a graceful exit.
installHandlers :: Core -> Async () -> IO ()
installHandlers core serverThread =
  let
    handle = do
      postLog (coreLogger core) "\nTermination sequence initiated ..."
      Core.postQuit core
      Async.cancel serverThread
    handler = Signals.CatchOnce handle
    blockSignals = Nothing
    installHandler signal = Signals.installHandler signal handler blockSignals
  in do
    void $ installHandler Signals.sigTERM
    void $ installHandler Signals.sigINT


main :: IO ()
main = do
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
  either putStrLn runCore eitherCore

  -- only stop logging when everything else has stopped
  Logger.postStop logger
  Async.wait loggerThread

runCore :: Core -> IO ()
runCore core = do
  let config = coreConfig core
  httpServer <- HttpServer.new core
  let wsServer = WebsocketServer.acceptConnection core

  -- start threads
  pops <- Async.async $ Core.runCommandLoop core
  sync <- Async.async $ Core.runSyncTimer core
  upds <- Async.async $ WebsocketServer.processUpdates core
  serv <- Async.async $ Server.runServer wsServer httpServer
  metrics <- Async.async
    $ forM_ (configMetricsEndpoint config) MetricsServer.runMetricsServer
  installHandlers core serv
  logAuthSettings config (coreLogger core)
  logQueueSettings config (coreLogger core)
  logSyncSettings config (coreLogger core)
  postLog (coreLogger core) "System online. ** robot sounds **"

  waitLog "core" (coreLogger core) pops
  waitLog "web sockets server" (coreLogger core) upds
  waitLog "http server" (coreLogger core) serv
  -- kill auxiliary threads when the main threads ended
  Async.cancel metrics
  Async.cancel sync

-- | Wait for an Async computation to exit and log unexpected exceptions.
waitLog :: Text -> Logger -> Async () -> IO ()
waitLog name logger action = Async.waitCatch action >>= handleLog where
  handleLog (Left exc)
    | Just (_ :: AsyncException) <- fromException exc = pure ()
    | otherwise = Logger.postLog logger $ name <> " stopped with an exception: " <> Text.pack (show exc)
  handleLog (Right _) = pure ()

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
