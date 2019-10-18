#!/usr/bin/env stack
-- stack --stack-yaml ../../client-haskell/stack.yaml runghc --package sqlite-simple --package aeson

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Test whether SQLite was installed with JSON support enabled by calling the "json" SQL
function.
-}
import Data.Aeson (FromJSON, ToJSON, Value, decode, encode)
import Data.Text (Text)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Data.Text.Lazy.Encoding

import qualified Data.ByteString.Lazy.Char8 as BL

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field


data JsonValue = JsonValue (Maybe Value) deriving (Generic, Show)

instance FromRow JsonValue where
  -- TODO: The "field" parser already parses to Text, so it's silly to then first encode this
  -- again only to then decode it, in order to parse it as JSON
  fromRow = JsonValue <$> (fmap (decode . encodeUtf8) field)

instance FromJSON JsonValue
instance ToJSON JsonValue


main :: IO ()
main = do
  conn <- open "test.db"
  -- execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r

  -- make sure that SQLite was compiled with JSON support enabled
  text <- query_ conn "SELECT json(1)" :: IO [Only Text]
  mapM_ print text

  -- parse the Text value to our own datatype instead
  jsonValue <- query_ conn "SELECT json(1)" :: IO [JsonValue]
  BL.putStrLn $ encode jsonValue

  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("{\"some\": 123}" :: String))

  close conn
