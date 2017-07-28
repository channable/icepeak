{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SubscriptionTreeSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import qualified Data.HashMap.Strict as HM

import Subscription (SubscriptionTree (..), empty, subscribe, unsubscribe)

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

    prop "adding clients is commutative" $ \ lpath rpath (lid :: Int) ->
      let
        rid = lid + 1 -- The ids need to be distinct.
        lconn = "dummy connection left" :: [Char]
        rconn = "dummy connection right" :: [Char]
        ladd = subscribe lpath lid lconn
        radd = subscribe rpath rid rconn
      in
        (ladd . radd) empty `shouldBe` (radd . ladd) empty

    prop "adding and removing a client is identity" $ \ path (cid :: Int) ->
      let
        conn = "dummy connection" :: String
        subUnsub = unsubscribe path cid . subscribe path cid conn
      in
        subUnsub empty `shouldBe` empty

    it "removing a non-existing client does nothing" $ do
      let
        path = []
        conn_id = 1 :: Int
      unsubscribe path conn_id (empty :: SubscriptionTree Int String)`shouldBe` empty

