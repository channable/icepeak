{-# LANGUAGE OverloadedStrings #-}
module Icepeak.Server.Store
(
  Modification (..),
  Path,
  Value,
  adjust,
  alter,
  modificationPath,
  applyModification,
  delete,
  insert,
  lookup,
  lookupOrNull,
)
where

import Data.Aeson (Value (..), (.=), (.:))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (lookup)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

type Path = [Text]

-- A modification operation.
data Modification
  = Put Path Value
  | Delete Path
  deriving (Eq, Show)

instance Aeson.ToJSON Modification where
  toJSON (Put path value) = Aeson.object
    [ "op" .= ("put" :: Text)
    , "path" .= path
    , "value" .= value
    ]
  toJSON (Delete path) = Aeson.object
    [ "op" .= ("delete" :: Text)
    , "path" .= path
    ]

instance Aeson.FromJSON Modification where
  parseJSON = Aeson.withObject "Modification" $ \v -> do
    op <- v .: "op"
    case op of
      "put" -> Put <$> v .: "path" <*> v .: "value"
      "delete" -> Delete <$> v .: "path"
      other -> Aeson.typeMismatch "Op" other

-- | Return the path that is touched by a modification.
modificationPath :: Modification -> Path
modificationPath op = case op of
  Put path _ -> path
  Delete path -> path

lookup :: Path -> Value -> Maybe Value
lookup path value =
  case path of
    [] -> Just value
    key : pathTail -> case value of
      Object dict -> KeyMap.lookup (Key.fromText key) dict >>= lookup pathTail
      _notObject -> Nothing

-- Look up a value, returning null if the path does not exist.
lookupOrNull :: Path -> Value -> Value
lookupOrNull path = fromMaybe Null . lookup path

-- | Execute a modification.
applyModification :: Modification -> Value -> Value
-- applyModification (Delete path) value = Store.delete path value
applyModification (Delete path) value = delete path value
applyModification (Put path newValue) value = insert path newValue value

-- Insert or overwrite a value at the given path, and create the path leading up to it if
-- it did not exist.
insert :: Path -> Value -> Value -> Value
insert path newValue value =
  case path of
    [] -> newValue
    key : pathTail -> Object $ case value of
      Object dict -> alter (Just . (insert pathTail newValue) . fromMaybe Null) (Key.fromText key) dict
      _notObject  -> KeyMap.singleton (Key.fromText key) $ insert pathTail newValue Null

-- Delete key at the given path. If the path is empty, return null.
delete :: Path -> Value -> Value
delete path value =
  case path of
    [] -> Null
    key : [] -> case value of
      Object dict -> Object $ KeyMap.delete (Key.fromText key) dict
      notObject   -> notObject
    key : pathTail -> case value of
      Object dict -> Object $ adjust (delete pathTail) (Key.fromText key) dict
      notObject   -> notObject


-- Note: We add two helper functions below, since Aeson does not implement them for the KeyMap type

-- Adjust a value at a specific key. When the key is not
-- a member of the KeyMap, the original KeyMap is returned.
adjust :: (v -> v) -> Key.Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
adjust f key keymap = runIdentity (KeyMap.alterF fMaybe key keymap)
  where
    fMaybe Nothing = pure Nothing
    fMaybe (Just v) = pure (Just (f v))

-- When the key is not a member of the KeyMap, then the key is inserted into the KeyMap.
-- When the 'f' function returns Nothing for a given key then the key is deleted from the KeyMap
-- and the altered KeyMap is returned.
alter :: (Maybe v -> Maybe v) -> Key.Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
alter f key keymap = runIdentity (KeyMap.alterF (pure . f) key keymap)
