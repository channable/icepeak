{-# LANGUAGE OverloadedStrings #-}
module PersistenceSpec (spec) where

import Data.Foldable
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8

import OrphanInstances ()
import Store (Modification (..))
import qualified Store

spec :: Spec
spec = do
  describe "Store.Modification" $ do
    prop "does not contain new lines when serialized" $ \op ->
      let jsonStr = Aeson.encode (op :: Modification)
      in '\n' `LBS8.notElem` jsonStr

    prop "round trips serialization" $ \op ->
      let jsonStr = Aeson.encode (op :: Modification)
          decoded = Aeson.decode jsonStr
      in Just op == decoded

  describe "Journaling" $ do
    prop "journal is idempotent" $ \ops initial ->
      let replay value = foldl' (flip Store.applyModification) value (ops :: [Modification])
      in replay initial == replay (replay initial)
