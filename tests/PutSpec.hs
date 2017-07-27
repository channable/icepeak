{-# LANGUAGE OverloadedStrings #-}

module PutSpec (spec) where

import Data.Aeson (Value (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.HashMap.Strict as HashMap

import MainLoop (Put (..), handlePut)

spec :: Spec
spec = do
  describe "MainLoop.handlePut" $ do

    it "creates an object when putting 'x' into Null" $
      let
        put = Put ["x"] (String "Robert")
        before = Null
        after = Object (HashMap.singleton "x" (String "Robert"))
      in
        handlePut put before `shouldBe` after
