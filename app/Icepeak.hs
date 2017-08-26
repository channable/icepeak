{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import Control.Monad (void)
import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (hGetContents, ByteString)
import Options.Applicative (execParser)
import Prelude hiding (log)
import System.IO (withFile, IOMode (..))

import qualified Control.Concurrent.Async as Async
import qualified System.Posix.Signals as Signals

import Config (configInfo, configDataFile)
import Core (Core (..))
import Logger (log, processLogRecords)

import qualified Core
import qualified HttpServer
import qualified Server
import qualified WebsocketServer

-- Instal SIGTERM and SIGINT handlers to do a graceful exit.
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

main :: IO ()
main = do
  config <- execParser configInfo
  -- load the persistent data from disk
  let filePath = configDataFile config
  eitherEncodedValue <- try $ withFile filePath ReadMode hGetContents

  case (eitherEncodedValue :: Either SomeException ByteString) of
      Left exc -> putStrLn $ "Failed to read the data from disk: " ++ show exc
      Right encodedValue -> case eitherDecodeStrict encodedValue of
          Left msg  -> error $ "Failed to decode the initial data: " ++ show msg
          Right value -> do
              core <- Core.newCore value config
              httpServer <- HttpServer.new core
              let wsServer = WebsocketServer.acceptConnection core
              pops <- Async.async $ Core.processOps core
              upds <- Async.async $ WebsocketServer.processUpdates core
              serv <- Async.async $ Server.runServer wsServer httpServer
              logger <- Async.async $ processLogRecords (coreLogRecords core)
              installHandlers core serv
              log "System online. ** robot sounds **" (coreLogRecords core)

              -- TODO: Log exceptions properly (i.e. non-interleaved)
              void $ Async.wait pops
              void $ Async.wait upds
              void $ Async.wait serv
              void $ Async.wait logger
