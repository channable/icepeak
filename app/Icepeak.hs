module Main where

import Control.Concurrent.Async (async, wait)
import Control.Monad (void)

import qualified Core

loop :: IO ()
loop = loop

main :: IO ()
main = do
  core <- Core.newCore
  puts <- async $ Core.processPuts core
  upds <- async $ Core.processUpdates core
  -- TODO: Start servers instead.
  void $ wait puts
  wait upds
