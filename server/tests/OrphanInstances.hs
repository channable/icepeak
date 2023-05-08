module OrphanInstances where

import Test.QuickCheck.Instances ()
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import qualified Test.QuickCheck.Gen as Gen

import Icepeak.Server.Store (Modification (..))
import Icepeak.Server.AccessControl

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
