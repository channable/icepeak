{-# LANGUAGE OverloadedStrings #-}
module SubscriptionTreeSpec (spec) where

import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, it, shouldBe)
import WebsocketServer (emptySubsTree, subscribe)

import qualified Data.UUID as UUID

spec :: Spec
spec = do
  describe "SubscriptionTree" $ do

    it "adds a client when calling subscribe" $ do
      let
        path = ["some", "path"]
        uuid = fromJust $ UUID.fromString "550e8400-e29b-41d4-a716-446655440000"
        conn = undefined
        after = emptySubsTree
      subscribe path uuid conn emptySubsTree `shouldBe` after
