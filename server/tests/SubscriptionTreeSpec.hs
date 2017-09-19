{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SubscriptionTreeSpec (spec) where

import Control.Monad.Writer (execWriter)
import Data.List (sortOn)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM

import Subscription (SubscriptionTree (..), broadcast', empty, subscribe, unsubscribe)

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

    do
      let
        conn1    , conn2    , conn3    , conn4 :: Int
        conn1 = 1; conn2 = 2; conn3 = 3; conn4 = 4

        root = SubscriptionTree (HM.fromList [(conn1, conn1)])
                                (HM.fromList [ ("foo", root_foo)
                                             , ("baz", root_baz) ])
        root_foo = SubscriptionTree (HM.fromList [(conn2, conn2)])
                                    (HM.fromList [("bar", root_foo_bar)])
        root_foo_bar = SubscriptionTree (HM.fromList [(conn3, conn3)]) HM.empty
        root_baz = SubscriptionTree (HM.fromList [(conn4, conn4)]) HM.empty

        value = AE.object ["foo" AE..= value_foo, "baz" AE..= value_baz]
        value_foo = AE.object ["bar" AE..= value_foo_bar]
        value_foo_bar = AE.Null
        value_baz = AE.object []

        broadcast'' path = sortOn fst . execWriter $ broadcast' path value root

      it "notifies everyone on root updates" $ do
        broadcast'' []
          `shouldBe` [ (conn1, value)
                     , (conn2, value_foo)
                     , (conn3, value_foo_bar)
                     , (conn4, value_baz)
                     ]

      it "notifies parents and children about updates" $ do
        broadcast'' ["foo"]
          `shouldBe` [ (conn1, value)
                     , (conn2, value_foo)
                     , (conn3, value_foo_bar)
                     ]

      it "notifies parents and children about updates" $ do
        broadcast'' ["foo", "bar"]
          `shouldBe` [ (conn1, value)
                     , (conn2, value_foo)
                     , (conn3, value_foo_bar)
                     ]
