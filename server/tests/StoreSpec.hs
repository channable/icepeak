{-# LANGUAGE OverloadedStrings #-}

module StoreSpec (spec) where

import Data.Aeson (Value (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import qualified Data.HashMap.Strict as HashMap

import OrphanInstances ()
import Store (Modification (..))
import qualified Store

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

  describe "Store.applyModification" $ do

    it "creates an object when putting 'x' into Null" $
      let
        put = Put ["x"] (String "Robert")
        before = Null
        after = Object $ HashMap.singleton "x" (String "Robert")
      in
        Store.applyModification put before `shouldBe` after

    it "overwrites a key when putting 'x' into {'x': ...}" $
      let
        put = Put ["x"] (String "Robert")
        before = Object $ HashMap.singleton "x" (String "Arian")
        after = Object $ HashMap.singleton "x" (String "Robert")
      in
        Store.applyModification put before `shouldBe` after

    it "adds a key when putting 'x' into {'y': ...}" $
      let
        put = Put ["x"] (String "Robert")
        before = Object $ HashMap.singleton "y" (String "Arian")
        after = Object $ HashMap.fromList [("x", String "Robert"), ("y", String "Arian")]
      in
        Store.applyModification put before `shouldBe` after

    it "creates a nested object when putting 'x/y' into Null" $
      let
        put = Put ["x", "y"] (String "Stefan")
        before = Null
        after = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Stefan"
      in
        Store.applyModification put before `shouldBe` after

    it "updates a nested object when putting 'x/y' into {'x': {'y': ...}}" $
      let
        put = Put ["x", "y"] (String "Stefan")
        before = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Radek"
        after = Object $ HashMap.singleton "x" $ Object $ HashMap.singleton "y" "Stefan"
      in
        Store.applyModification put before `shouldBe` after

    it "adds a nested key when putting 'x/y' into {'x': {'y': ...}, 'z': ...}" $
      let
        put = Put ["x", "y"] (String "Stefan")
        before = Object $ HashMap.fromList [("x", Object $ HashMap.singleton "y" "Nuno"), ("z", Null)]
        after = Object $ HashMap.fromList [("x", Object $ HashMap.singleton "y" "Stefan"), ("z", Null)]
      in
        Store.applyModification put before `shouldBe` after

    prop "is idempotent" $ \op value ->
      let apply = Store.applyModification op
      in apply value == apply (apply value)

    it "passes this noop test. Wut Cirrus, why do you pass?" $ do
      pure ()
