#!/usr/bin/env stack
-- stack --stack-yaml ../../client-haskell/stack.yaml runghc --package QuickCheck --package quickcheck-text --package icepeak-client

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (fail)

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Arbitrary ()
import Icepeak.Client (Client (..), Config (..), setAtLeaf)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.QuickCheck.Arbitrary (arbitrary)
import Text.Read (readEither)

import qualified Network.HTTP.Client as HTTP
import qualified Test.QuickCheck.Gen as Gen

main :: IO ()
main = do
  args <- getArgs
  print args
  case args of
    [count] -> either fail main' (readEither count)
    _ -> fail "usage: stress_test.hs <count>"
  where fail msg = do { hPutStrLn stderr msg; exitFailure }

main' :: Int -> IO ()
main' count = do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  let client = Client httpManager (Config "localhost" 3000 Nothing)
  let putRandomPayload i = do
        putStr $ "Test #" ++ show i ++ " ... "
        path :: [Text] <- Gen.generate arbitrary
        leaf :: Text <- Gen.generate arbitrary
        status <- setAtLeaf client path leaf
        print status
  traverse_ putRandomPayload [1..count]
