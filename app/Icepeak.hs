{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Control.Concurrent.Async
import Data.Aeson (eitherDecode, Value (..))
import Prelude hiding (log)
import System.IO (withFile, IOMode (..))

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Lazy as BS
import qualified System.Posix.Signals as Signals

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
  maybeValue <- withFile "icepeak.json" ReadMode BS.hGetContents
  -- load the persistent data from disk
  -- maybeValue <- BS.readFile "icepeak.json"

  let value = case eitherDecode maybeValue of
                Left _msg  -> Object mempty
                Right obj  -> obj
  core <- Core.newCore value
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
