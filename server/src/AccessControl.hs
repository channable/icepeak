{-# LANGUAGE OverloadedStrings #-}
-- | This module defines the kinds of permissions used in icepeak and provides
-- functions checking for sufficient permissions for certain operations.
module AccessControl
       ( AccessMode (..)
       , AuthPath (..)
       , IcepeakClaim (..)
       , Path
       , allowEverything
       , accessModeToText
       , textToAccessMode
       , isAuthorizedByClaim
       ) where

import           Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.List  as List
import           Data.Text  (Text)

import           Store      (Path)

-- * Claim datatypes

-- | Defines the structure of a JWT claim for Icepeak.
data IcepeakClaim = IcepeakClaim
  { icepeakClaimWhitelist :: [AuthPath]
    -- ^ The whitelist containing all authorizations.
  } deriving (Read, Show, Eq, Ord)

data AuthPath = AuthPath
  { authPathPrefix :: Path
    -- ^ The prefix of all the paths to which this authorization applies.
  , authPathModes  :: [AccessMode]
    -- ^ The modes that are authorized on this path prefix.
  } deriving (Read, Show, Eq, Ord)

-- | Different modes for accessing the JSON store
data AccessMode = ModeRead | ModeWrite
  deriving (Read, Show, Eq, Ord, Enum, Bounded)


-- | A claim that allows all operations.
allowEverything :: IcepeakClaim
allowEverything = IcepeakClaim [AuthPath [] [minBound..maxBound]]

-- * Authorization

-- | Check whether accessing the given path with the given mode is authorized by
-- the supplied claim.
isAuthorizedByClaim :: IcepeakClaim -> Path -> AccessMode -> Bool
isAuthorizedByClaim claim path mode = any allows (icepeakClaimWhitelist claim) where
  allows (AuthPath prefix modes) = List.isPrefixOf prefix path && mode `elem` modes


-- * JSON encoding and decoding

accessModeToText :: AccessMode -> Text
accessModeToText mode = case mode of
    ModeRead   -> "read"
    ModeWrite -> "write"

textToAccessMode :: Text -> Maybe AccessMode
textToAccessMode mode
  | mode == "read" = Just ModeRead
  | mode == "write" = Just ModeWrite
  | otherwise = Nothing

instance Aeson.ToJSON AccessMode where
  toJSON = Aeson.String . accessModeToText

instance Aeson.FromJSON AccessMode where
  parseJSON = Aeson.withText "mode string" $ \txt -> case textToAccessMode txt of
    Nothing -> fail "Invalid mode value."
    Just m  -> pure m

instance Aeson.ToJSON AuthPath where
  toJSON (AuthPath prefix modes) = Aeson.object
    [ "prefix" .= prefix
    , "modes" .= modes ]

instance Aeson.FromJSON AuthPath where
  parseJSON = Aeson.withObject "path and modes" $ \v -> AuthPath
    <$> v .: "prefix"
    <*> v .: "modes"

instance Aeson.ToJSON IcepeakClaim where
  toJSON claim = Aeson.object
    [ "version"   .= (1 :: Int)
    , "whitelist" .= icepeakClaimWhitelist claim
    ]

instance Aeson.FromJSON IcepeakClaim where
  parseJSON = Aeson.withObject "icepeak claim" $ \v -> do
    version <- v .: "version"
    if version == (1 :: Int)
      then IcepeakClaim <$> v .: "whitelist"
      else fail "unsupported version"
