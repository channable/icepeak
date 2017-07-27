module Main where

import Data.Void (absurd)

import qualified MainLoop

main :: IO ()
main = do
  core <- MainLoop.newCore
  absurd <$> MainLoop.mainLoop core
