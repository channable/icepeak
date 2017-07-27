module Main where

import Control.Concurrent.Async (async, wait)
import Control.Monad (void)

import qualified System.Posix.Signals as Signals

import Core (Core)

import qualified Core

-- Instal SIGTERM and SIGINT handlers to do a graceful exit.
installHandlers :: Core -> IO ()
installHandlers core =
  let
    handle = do
      Core.postQuit core
      putStrLn "\nTermination sequence initiated ..."
    handler = Signals.CatchOnce handle
    blockSignals = Nothing
    installHandler signal = Signals.installHandler signal handler blockSignals
  in do
    void $ installHandler Signals.sigTERM
    void $ installHandler Signals.sigINT

main :: IO ()
main = do
  core <- Core.newCore
  installHandlers core
  puts <- async $ Core.processPuts core
  upds <- async $ Core.processUpdates core
  -- TODO: Start servers.
  putStrLn "System online. ** robot sounds **"
  void $ wait puts
  void $ wait upds
  putStrLn "okdoei"
