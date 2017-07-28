{-# LANGUAGE OverloadedStrings #-}
module SubscriptionTreeSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Subscription (SubscriptionTree (..), empty, subscribe)

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
