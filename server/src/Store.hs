{-# LANGUAGE OverloadedStrings #-}
module Store
(
  Modification (..),
  Path,
  Value,
  modificationPath,
  applyModification,
  delete,
  insert,
  lookup,
  lookupOrNull,
)
where

import Data.Aeson (Value (..), (.=), (.:))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (lookup)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HashMap

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
    , "val" .= value
    ]
  toJSON (Delete path) = Aeson.object
    [ "op" .= ("del" :: Text)
    , "path" .= path
    ]

instance Aeson.FromJSON Modification where
  parseJSON = Aeson.withObject "Modification" $ \v -> do
    op <- v .: "op"
    case op of
      "put" -> Put <$> v .: "path" <*> v .: "val"
      "del" -> Delete <$> v .: "path"
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
      Object dict -> HashMap.lookup key dict >>= lookup pathTail
      _notObject -> Nothing

-- Look up a value, returning null if the path does not exist.
lookupOrNull :: Path -> Value -> Value
lookupOrNull path = fromMaybe Null . lookup path

-- | Execute a modification.
applyModification :: Modification -> Value -> Value
applyModification (Delete path) value = Store.delete path value
applyModification (Put path newValue) value = Store.insert path newValue value

-- Overwrite a value at the given path, and create the path leading up to it if
-- it did not exist.
insert :: Path -> Value -> Value -> Value
insert path newValue value =
  case path of
    [] -> newValue
    key : pathTail -> Object $ case value of
      Object dict -> HashMap.alter (Just . (insert pathTail newValue) . fromMaybe Null) key dict
      _notObject  -> HashMap.singleton key $ insert pathTail newValue Null

-- Delete key at the given path. If the path is empty, return null.
delete :: Path -> Value -> Value
delete path value =
  case path of
    [] -> Null
    key : [] -> case value of
      Object dict -> Object $ HashMap.delete key dict
      notObject   -> notObject
    key : pathTail -> case value of
      Object dict -> Object $ HashMap.adjust (delete pathTail) key dict
      notObject   -> notObject
