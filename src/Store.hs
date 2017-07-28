module Store
(
  Path,
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
lookup path value = case path of
  [] -> Just value
  key : pathTail -> case value of
    Object dict -> HashMap.lookup key dict >>= lookup pathTail
    _notObject -> Nothing

-- Look up a value, returning null if the path does not exist.
lookupOrNull :: Path -> Value -> Value
lookupOrNull path = fromMaybe Null . lookup path
