{-# LANGUAGE OverloadedStrings #-}

module StoreSpec (spec) where

import Data.Aeson (Value (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))

import qualified Data.HashMap.Strict as HashMap
import qualified Test.QuickCheck.Gen as Gen

import qualified Store

instance Arbitrary Value where
  arbitrary = Gen.oneof
    [ Object <$> arbitrary
    , Array  <$> arbitrary
    , String <$> arbitrary
    , Bool   <$> arbitrary
    , pure Null
    ]

spec :: Spec
spec = do
  describe "Store.insert" $ do

    it "creates an object when putting 'x' into Null" $
      let
        before = Null
        after = Object $ HashMap.singleton "x" (String "Robert")
      in
        Store.insert ["x"] (String "Robert") before `shouldBe` after

    it "overwrites a key when putting 'x' into {'x': ...}" $
      let
        before = Object $ HashMap.singleton "x" (String "Arian")
        after = Object $ HashMap.singleton "x" (String "Robert")
      in
        Store.insert ["x"] (String "Robert") before `shouldBe` after

    it "adds a key when putting 'x' into {'y': ...}" $
      let
        before = Object $ HashMap.singleton "y" (String "Arian")
        after = Object $ HashMap.fromList [("x", String "Robert"), ("y", String "Arian")]
      in
        Store.insert ["x"] (String "Robert") before `shouldBe` after

    it "creates a nested object when putting 'x/y' into Null" $
      let
        before = Null
        after = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Stefan"
      in
        Store.insert ["x", "y"] (String "Stefan") before `shouldBe` after

    it "updates a nested object when putting 'x/y' into {'x': {'y': ...}}" $
      let
        before = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Radek"
        after = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Stefan"
      in
        Store.insert ["x", "y"] (String "Stefan") before `shouldBe` after

    it "adds a nested key when putting 'x/y' into {'x': {'y': ...}, 'z': ...}" $
      let
        before = Object $ HashMap.fromList [("x", Object $ HashMap.singleton "y" "Nuno"), ("z", Null)]
        after = Object $ HashMap.fromList [("x", Object $ HashMap.singleton "y" "Stefan"), ("z", Null)]
      in
        Store.insert ["x", "y"] (String "Stefan") before `shouldBe` after

  describe "Store" $ do

    prop "returns None after (lookup . delete . insert) in Null" $ \ path value ->
      let
        lkupDelIns = Store.lookup path . Store.delete path . Store.insert path value
      in
        if path == []
          then lkupDelIns Null `shouldBe` (Just Null)
          else lkupDelIns Null `shouldBe` Nothing

    prop "returns None after (lookup . delete . insert) in anything" $ \ path value before ->
      let
        lkupDelIns = Store.lookup path . Store.delete path . Store.insert path value
      in
        if path == []
          then lkupDelIns before `shouldBe` (Just Null)
          else lkupDelIns before `shouldBe` Nothing
