#!/usr/bin/env stack
-- stack --stack-yaml ../../client-haskell/stack.yaml runghc --package sqlite-simple

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

{- | This test does nothing yet.
-}
import Database.SQLite.Simple

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "test.db"
  -- execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  close conn
