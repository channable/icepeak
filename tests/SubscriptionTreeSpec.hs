{-# LANGUAGE OverloadedStrings #-}
module SubscriptionTreeSpec (spec) where

import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, it, shouldBe)
import Subscription (SubscriptionTree (..), empty, subscribe)

import qualified Data.UUID as UUID
import qualified Data.HashMap.Strict as HM

spec :: Spec
spec = do
  describe "SubscriptionTree" $ do

    it "adds a client listening to the root when calling subscribe with []" $ do
      let
        path = []
        uuid = fromJust $ UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
        conn = "dummy connection" :: String
        after = SubscriptionTree (HM.fromList [(uuid, conn)]) HM.empty
      subscribe path uuid conn empty `shouldBe` after

    it "adds a client listening to \"some\" when calling subscribe with [\"some\"]" $ do
      let
        path = ["some"]
        uuid = fromJust $ UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
        conn = "dummy connection" :: [Char]
        after = SubscriptionTree
                  HM.empty
                  (HM.fromList [("some", SubscriptionTree (HM.fromList [(uuid,conn)]) HM.empty)])
      subscribe path uuid conn empty `shouldBe` after
