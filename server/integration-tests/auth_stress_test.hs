#!/usr/bin/env stack
-- stack --stack-yaml ../../client-haskell/stack.yaml runghc --package QuickCheck --package quickcheck-text

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | This test first sends a number of write requests that should be accepted by
   the server, followed by the same number of requests that should be accepted.
-}
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

import qualified Data.ByteString as SBS
import qualified Network.HTTP.Client as HTTP
import qualified Test.QuickCheck.Gen as Gen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [count] -> either fail main' (readEither count)
    _ -> fail "Usage: ./auth_stress_test.hs <count>"
  where fail msg = do { hPutStrLn stderr msg; exitFailure }

main' :: Int -> IO ()
main' count = do
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  let client = Client httpManager (Config "localhost" 3000 (Just testToken))
  let putRandomPayload prefix i = do
        putStr $ "Test #" ++ show i ++ " ... "
        path :: [Text] <- Gen.generate arbitrary
        leaf :: Text <- Gen.generate arbitrary
        status <- setAtLeaf client (prefix ++ path) leaf
        print status
  traverse_ (putRandomPayload ["bar", "baz"]) [1..count]
  traverse_ (putRandomPayload ["foo"]) [count+1..2*count]

testToken :: SBS.ByteString
testToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpY2VwZWFrIjp7IndoaXRlbGlzdCI6W3sicHJlZml4IjpbImZvbyJdLCJtb2RlcyI6WyJyZWFkIl19LHsicHJlZml4IjpbImJhciIsImJheiJdLCJtb2RlcyI6WyJyZWFkIiwid3JpdGUiXX1dLCJ2ZXJzaW9uIjoxfX0.KeOHokJZuN-sPmIIGSDgRQFBtT4x-hoGgTtxhm2R5P8"
