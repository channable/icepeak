{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.Async
import Control.Monad (forM, void, when)
import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Options.Applicative (execParser)
import Prelude hiding (log)
import System.Environment (getEnvironment)

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as Text
import qualified Prometheus
import qualified Prometheus.Metric.GHC
import qualified System.Posix.Signals as Signals

import Config (Config (..), configInfo)
import Core (Core (..))
import Logger (log, processLogRecords, LogQueue)

import qualified Core
import qualified HttpServer
import qualified Server
import qualified WebsocketServer
import qualified Metrics
import qualified MetricsServer

-- Install SIGTERM and SIGINT handlers to do a graceful exit.
installHandlers :: Core -> Async () -> IO ()
installHandlers core serverThread =
  let
    handle = do
      log "\nTermination sequence initiated ..." (coreLogRecords core)
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

  -- setup metrics if enabled
  icepeakMetrics <- forM (configMetricsEndpoint config) $ const $ do
    void $ Prometheus.register Prometheus.Metric.GHC.ghcMetrics
    Metrics.createAndRegisterIcepeakMetrics

  eitherCore <- Core.newCore config icepeakMetrics
  either putStrLn runCore eitherCore

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
  logger <- Async.async $ processLogRecords (coreLogRecords core)
  metrics <- Async.async
    $ forM_ (configMetricsEndpoint config) MetricsServer.runMetricsServer
  installHandlers core serv
  logAuthSettings config (coreLogRecords core)
  logQueueSettings config (coreLogRecords core)
  logSyncSettings config (coreLogRecords core)
  log "System online. ** robot sounds **" (coreLogRecords core)

  -- TODO: Log exceptions properly (i.e. non-interleaved)
  void $ Async.wait pops
  void $ Async.wait upds
  void $ Async.wait serv
  void $ Async.wait logger
  -- kill auxiliary threads when the main threads ended
  Async.cancel metrics
  Async.cancel sync

logAuthSettings :: Config -> LogQueue -> IO ()
logAuthSettings cfg queue
  | configEnableJwtAuth cfg = case configJwtSecret cfg of
      Just _ -> log "JWT authorization enabled and secret provided, tokens will be verified." queue
      Nothing -> log "JWT authorization enabled but no secret provided, tokens will NOT be verified." queue
  | otherwise = case configJwtSecret cfg of
      Just _ -> log "WARNING a JWT secret has been provided, but JWT authorization is disabled." queue
      Nothing -> log "JWT authorization disabled." queue

logQueueSettings :: Config -> LogQueue -> IO ()
logQueueSettings cfg queue =
  log ("Queue capacity is set to " <> Text.pack (show (configQueueCapacity cfg)) <> ".") queue

logSyncSettings :: Config -> LogQueue -> IO ()
logSyncSettings cfg queue = case configSyncIntervalMicroSeconds cfg of
  Nothing -> do
    log "Sync: Persisting after every modification" queue
    when (configEnableJournaling cfg) $ do
      log "Journaling has no effect when periodic syncing is disabled" queue
  Just musecs -> do
    log ("Sync: every " <> Text.pack (show musecs) <> " microseconds.") queue
    when (configEnableJournaling cfg) $ do
      log "Journaling enabled" queue
