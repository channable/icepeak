{-# LANGUAGE OverloadedStrings #-}

module CoreSpec (spec) where

import Data.Aeson (Value (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.HashMap.Strict as HashMap

import Core (Op (..), handleOp)

spec :: Spec
spec = do
  describe "Core.handleOp" $ do

    it "creates an object when putting 'x' into Null" $
      let
        put = Put ["x"] (String "Robert")
        before = Null
        after = Object $ HashMap.singleton "x" (String "Robert")
      in
        handleOp put before `shouldBe` after

    it "overwrites a key when putting 'x' into {'x': ...}" $
      let
        put = Put ["x"] (String "Robert")
        before = Object $ HashMap.singleton "x" (String "Arian")
        after = Object $ HashMap.singleton "x" (String "Robert")
      in
        handleOp put before `shouldBe` after

    it "adds a key when putting 'x' into {'y': ...}" $
      let
        put = Put ["x"] (String "Robert")
        before = Object $ HashMap.singleton "y" (String "Arian")
        after = Object $ HashMap.fromList [("x", String "Robert"), ("y", String "Arian")]
      in
        handleOp put before `shouldBe` after

    it "creates a nested object when putting 'x/y' into Null" $
      let
        put = Put ["x", "y"] (String "Stefan")
        before = Null
        after = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Stefan"
      in
        handleOp put before `shouldBe` after

    it "updates a nested object when putting 'x/y' into {'x': {'y': ...}}" $
      let
        put = Put ["x", "y"] (String "Stefan")
        before = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Radek"
        after = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Stefan"
      in
        handleOp put before `shouldBe` after

    it "adds a nested key when putting 'x/y' into {'x': {'y': ...}, 'z': ...}" $
      let
        put = Put ["x", "y"] (String "Stefan")
        before = Object $ HashMap.fromList [("x", Object $ HashMap.singleton "y" "Nuno"), ("z", Null)]
        after = Object $ HashMap.fromList [("x", Object $ HashMap.singleton "y" "Stefan"), ("z", Null)]
      in
        handleOp put before `shouldBe` after
