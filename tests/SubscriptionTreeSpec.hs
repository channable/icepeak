{-# LANGUAGE OverloadedStrings #-}
module SubscriptionTreeSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Subscription (SubscriptionTree (..), empty, subscribe, unsubscribe)

import qualified Data.HashMap.Strict as HM

spec :: Spec
spec = do
  describe "SubscriptionTree" $ do

    it "adds a client listening to the root when calling subscribe with []" $ do
      let
        path = []
        conn_id = 1 :: Int
        conn = "dummy connection" :: String
        after = SubscriptionTree (HM.fromList [(conn_id, conn)]) HM.empty
      subscribe path conn_id conn empty `shouldBe` after

    it "adds a client listening to \"some\" when calling subscribe with [\"some\"]" $ do
      let
        path = ["some"]
        conn_id = 1 :: Int
        conn = "dummy connection" :: [Char]
        after = SubscriptionTree
                  HM.empty
                  (HM.fromList [("some", SubscriptionTree (HM.fromList [(conn_id,conn)]) HM.empty)])
      subscribe path conn_id conn empty `shouldBe` after

    it "adds two clients: ones listening to the root and one to \"some\"" $ do
      let
        root = []
        path = ["some"]
        conn_id = 1 :: Int
        conn_id2 = 2 :: Int
        conn = "dummy connection" :: [Char]
        conn2 = "dummy connection2" :: [Char]
        after = SubscriptionTree
                  (HM.fromList [(conn_id, conn)])
                  (HM.fromList [("some", SubscriptionTree (HM.fromList [(conn_id2,conn2)]) HM.empty)])
      subscribe root conn_id conn (subscribe path conn_id2 conn2 empty) `shouldBe` after

    it "adding clients is associative" $ do
      let
        root = []
        path = ["some"]
        conn_id = 1 :: Int
        conn_id2 = 2 :: Int
        conn = "dummy connection" :: [Char]
        conn2 = "dummy connection2" :: [Char]
        lhs = subscribe root conn_id conn (subscribe path conn_id2 conn2 empty)
        rhs = subscribe path conn_id2 conn2 (subscribe root conn_id conn empty)
      lhs `shouldBe` rhs

    it "adding and removing a client is identity" $ do
      let
        path = []
        conn_id = 1 :: Int
        conn = "dummy connection" :: String
      unsubscribe path conn_id (subscribe path conn_id conn empty) `shouldBe` empty

    it "removing a non-existing client does nothing" $ do
      let
        path = []
        conn_id = 1 :: Int
      unsubscribe path conn_id (empty :: SubscriptionTree Int String)`shouldBe` empty

