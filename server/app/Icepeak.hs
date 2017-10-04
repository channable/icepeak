{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import Control.Monad (forM, void)
import Data.Aeson (eitherDecodeStrict, Value)
import Data.ByteString (hGetContents, ByteString)
import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Options.Applicative (execParser)
import Prelude hiding (log)
import System.Environment (getEnvironment)
import System.IO (withFile, IOMode (..))

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as SBS
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
      Core.postQuit core
      Async.cancel serverThread
      log "\nTermination sequence initiated ..." (coreLogRecords core)
    handler = Signals.CatchOnce handle
    blockSignals = Nothing
    installHandler signal = Signals.installHandler signal handler blockSignals
  in do
    void $ installHandler Signals.sigTERM
    void $ installHandler Signals.sigINT


readValue :: FilePath -> Maybe Metrics.IcepeakMetrics -> IO Value
readValue filePath metrics = do
  eitherEncodedValue <- try $ withFile filePath ReadMode hGetContents
  case (eitherEncodedValue :: Either SomeException ByteString) of
      Left exc -> error $ "Failed to read the data from disk: " ++ show exc
      Right encodedValue -> do
        forM_ metrics $ Metrics.setDataSize (SBS.length encodedValue)
        case eitherDecodeStrict encodedValue of
          Left msg  -> error $ "Failed to decode the initial data: " ++ show msg
          Right value -> return value

main :: IO ()
main = do
  env <- getEnvironment
  config <- execParser (configInfo env)

  -- setup metrics if enabled
  icepeakMetrics <- forM (configMetricsEndpoint config) $ const $ do
    void $ Prometheus.register Prometheus.Metric.GHC.ghcMetrics
    Metrics.createAndRegisterIcepeakMetrics

  -- load the persistent data from disk
  let filePath = configDataFile config
  value <- readValue filePath icepeakMetrics
  core <- Core.newCore value config icepeakMetrics
  httpServer <- HttpServer.new core
  let wsServer = WebsocketServer.acceptConnection core

  -- start threads
  pops <- Async.async $ Core.processOps core
  upds <- Async.async $ WebsocketServer.processUpdates core
  serv <- Async.async $ Server.runServer wsServer httpServer
  logger <- Async.async $ processLogRecords (coreLogRecords core)
  metrics <- Async.async
    $ forM_ (configMetricsEndpoint config) MetricsServer.runMetricsServer
  installHandlers core serv
  logAuthSettings config (coreLogRecords core)
  logQueueSettings config (coreLogRecords core)
  log "System online. ** robot sounds **" (coreLogRecords core)

  -- TODO: Log exceptions properly (i.e. non-interleaved)
  void $ Async.wait pops
  void $ Async.wait upds
  void $ Async.wait serv
  void $ Async.wait logger
  -- kill metrics endpoint when the main threads ended
  Async.cancel metrics

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
