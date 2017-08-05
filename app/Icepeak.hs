{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Control.Concurrent.Async
import Prelude hiding (log)

import qualified System.Posix.Signals as Signals
import qualified Control.Concurrent.Async as Async

import Core (Core (..))
import Logger (log, processLogRecords)

import qualified Core
import qualified HttpServer
import qualified Server
import qualified WebsocketServer


main :: IO ()
main = do
  core <- Core.newCore
  httpServer <- HttpServer.new core
  let wsServer = WebsocketServer.acceptConnection core
  pops <- Async.async $ Core.processOps core
  upds <- Async.async $ WebsocketServer.processUpdates core
  serv <- Async.async $ Server.runServer wsServer httpServer
  logger <- Async.async $ processLogRecords (coreLogRecords core)
  installSystemSignalsHandlers core serv
  log "System online. ** robot sounds **" (coreLogRecords core)
  void $ Async.wait pops
  void $ Async.wait upds
  void $ Async.wait serv
  void $ Async.wait logger

installSystemSignalsHandlers :: Core -> Async () -> IO ()
installSystemSignalsHandlers core serverThread =
  mapM_ installHandler [Signals.sigTERM, Signals.sigINT]
  where
    handle = do
      Core.postQuit core
      Async.cancel serverThread
      log "\nTermination sequence initiated ..." (coreLogRecords core)
    handler = Signals.CatchOnce handle
    blockSignals = Nothing
    installHandler signal = Signals.installHandler signal handler blockSignals
