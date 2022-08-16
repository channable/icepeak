module OrphanInstances where

import Data.Aeson (Value (..))
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import qualified Test.QuickCheck.Gen as Gen

import Store (Modification (..))
import AccessControl

instance Arbitrary AccessMode where
  arbitrary = Gen.elements [minBound..maxBound]

instance Arbitrary AuthPath where
  arbitrary = AuthPath <$> arbitrary <*> arbitrary

instance Arbitrary IcepeakClaim where
  arbitrary = IcepeakClaim <$> arbitrary

instance Arbitrary Modification where
  arbitrary = Gen.oneof
    [ Put <$> arbitrary <*> arbitrary
    , Delete <$> arbitrary
    ]