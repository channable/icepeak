module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Void (absurd)

import qualified Core

loop :: IO ()
loop = loop

main :: IO ()
main = do
  core <- Core.newCore
  void $ forkIO (absurd <$> Core.processPuts core)
  void $ forkIO (absurd <$> Core.processUpdates core)
  -- TODO: Start servers instead.
  loop
