module Main where

import Control.Monad (void)
import Data.Void (absurd)
import Control.Concurrent (forkIO)

import qualified Core

main :: IO ()
main = do
  core <- Core.newCore
  void $ forkIO (absurd <$> Core.processPuts core)
  void $ forkIO (absurd <$> Core.processUpdates core)
