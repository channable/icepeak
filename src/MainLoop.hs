module MainLoop
(
  Put (..),
  handlePut,
  mainLoop,
  newState,
)
where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import Data.Aeson (Value (..))
import Data.Void (Void)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as HashMap

-- Put is a command to put a value at a given path.
data Put = Put [Text] Value

data State = State
  { stValue :: TVar Value
  , stQueue :: TBQueue Put
  }

newState :: IO State
newState = do
  tvalue <- newTVarIO Null
  tqueue <- newTBQueueIO 128
  pure (State tvalue tqueue)

-- Execute a "put" operation.
handlePut :: Put -> Value -> Value
handlePut (Put path newValue) value = case path of
  [] -> newValue
  key : pathTail ->
    let
      putInner = handlePut (Put pathTail newValue)
      newDict = case value of
        Object dict -> HashMap.alter (Just . putInner . fromMaybe Null) key dict
        _notObject  -> HashMap.singleton key (putInner Null)
    in
      Object newDict

mainLoop :: State -> IO Void
mainLoop state =
  mainLoop state
