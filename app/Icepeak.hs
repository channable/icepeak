module Main where

import Data.Void (absurd)

import qualified MainLoop

main :: IO ()
main = do
  initialState <- MainLoop.newState
  absurd <$> MainLoop.mainLoop initialState
