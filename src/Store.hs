module Store
(
  Path,
  delete,
  insert,
  lookup,
  lookupOrNull,
)
where

import Data.Aeson (Value (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Prelude hiding (lookup)

import qualified Data.HashMap.Strict as HashMap

type Path = [Text]

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
