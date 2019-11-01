#!/usr/bin/env stack
-- stack --stack-yaml ../../client-haskell/stack.yaml runghc --package sqlite-simple --package aeson

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Test inserting and reading a JSON value as type BLOB in Sqlite.
-}
import Data.Aeson (Value, eitherDecode)
import Data.Text (Text)
import Database.SQLite.Simple

import qualified Data.ByteString.Lazy as BSL

data TestField = TestField BSL.ByteString deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field


main :: IO ()
main = do
  conn <- open "test.db"

  -- we use the BLOB type here, since we want to store a bytestring directly, and we don't want
  -- sqlite-simple to do any decoding, since we can better do this directly using Aeson.decode
  execute_ conn "CREATE TABLE IF NOT EXISTS test (value BLOB)"

  -- make sure that SQLite was compiled with JSON support enabled
  text <- query_ conn "SELECT json(1)" :: IO [Only Text]
  mapM_ print text

  -- update the single field in the test database
  executeNamed conn "UPDATE test SET value = :value" [":value" := ("{}" :: BSL.ByteString)]

  -- make sure that we can read the JSON value back from the database
  (TestField bytestring):_ <- query_ conn "SELECT * from test" :: IO [TestField]
  let value = eitherDecode bytestring :: (Either String Value)
  print bytestring
  print value

  close conn
