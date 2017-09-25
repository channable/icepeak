{-# LANGUAGE OverloadedStrings #-}
module AccessControlSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Aeson as Aeson
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import AccessControl
import OrphanInstances ()

spec :: Spec
spec = do
  describe "AccessControl" $ do
    let testClaim = IcepeakClaim
          [ AuthPath ["foo"] [ModeRead]
          , AuthPath ["foo", "bar"] [ModeWrite]
          , AuthPath ["a", "b"] [ModeRead, ModeWrite]
          ]

    prop "empty whitelist disallows everything" $ \path mode ->
      let emptyClaim = IcepeakClaim []
      in not $ isAuthorizedByClaim emptyClaim path mode

    prop "empty path in whitelist allows everything" $ \path mode ->
      let allClaim = IcepeakClaim [AuthPath [] [ModeRead, ModeWrite]]
      in isAuthorizedByClaim allClaim path mode

    prop "allowEverything claim allows everything" $ isAuthorizedByClaim allowEverything

    it "should allow access to sub-paths" $ do
      isAuthorizedByClaim testClaim ["foo", "1"] ModeRead `shouldBe` True
      isAuthorizedByClaim testClaim ["foo", "bar"] ModeRead `shouldBe` True
      isAuthorizedByClaim testClaim ["foo", "bar", "1"] ModeWrite `shouldBe` True

    it "should disallow access to non-whitelisted paths" $ do
      isAuthorizedByClaim testClaim ["foo", "1"] ModeWrite `shouldBe` False
      isAuthorizedByClaim testClaim ["a"] ModeRead `shouldBe` False
      isAuthorizedByClaim testClaim ["a", "c"] ModeWrite `shouldBe` False

    prop "serializing and deserializing claim is identity" $ \claim ->
      Aeson.fromJSON (Aeson.toJSON claim) == Aeson.Success (claim :: IcepeakClaim)
