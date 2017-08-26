{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Control.Concurrent.Async
import Data.Aeson (eitherDecodeStrict)
-- import Data.ByteString.Lazy (hGetContents)
import Data.ByteString (hGetContents)
import Options.Applicative (execParser)
import Prelude hiding (log)
import System.IO (withFile, IOMode (..))

import qualified Control.Concurrent.Async as Async
import qualified System.Posix.Signals as Signals

import Config (configInfo, cDataFile)
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
  let filePath = cDataFile config
  encodedValue <- withFile (cDataFile config) ReadMode hGetContents

  case eitherDecodeStrict encodedValue of
      Left msg  -> error $ "Failed to load the initial data in " ++ filePath ++
                           ": " ++ msg
      Right value -> do
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
