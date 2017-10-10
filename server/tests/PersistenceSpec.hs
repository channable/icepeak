{-# LANGUAGE OverloadedStrings #-}
module PersistenceSpec (spec) where

--import Data.Aeson (Value (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Instances ()
--import Test.QuickCheck.Arbitrary (Arbitrary (..))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
--import qualified Data.HashMap.Strict as HashMap
--import qualified Test.QuickCheck.Gen as Gen

import OrphanInstances ()
import Store (Modification (..))
import qualified Persistence
--import qualified Store

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
    prop "does not crash when replaying faulty journal" $ \ops initial cutoff ->
      let journalLines = map Aeson.encode (ops :: [Modification])
          journal = LBS8.intercalate "\n" journalLines
          brokenJournal = LBS8.take (LBS8.length journal - cutoff) journal
          brokenLines = map LBS8.toStrict $ LBS8.split '\n' brokenJournal
          (errs, restoredOps) = Persistence.parseJournalData brokenLines
          val = Persistence.replayModifications restoredOps initial
      in errs `seq` val `seq` True

    prop "parses valid journal without errors" $ \ops ->
      let journalLines = map (LBS8.toStrict . Aeson.encode) (ops :: [Modification])
          (errs, restoredOps) = Persistence.parseJournalData journalLines
      in null errs && restoredOps == ops
