module MainLoop
(
  Put (..),
  handlePut,
  mainLoop,
  newState,
)
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Data.Aeson (Value (..))
import Data.Void (Void)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import qualified Data.HashMap.Strict as HashMap

-- Put is a command to put a value at a given path.
data Put = Put [Text] Value

-- The main value has been updated at the given path. The payload contains the
-- entire new value. (So not only the inner value at the updated path.)
data Updated = Updated [Text] Value

data State = State
  { stateValue :: TVar Value
  , stateQueue :: TBQueue Put
  , stateUpdates :: TBQueue Updated
  }

newState :: IO State
newState = do
  tvalue <- newTVarIO Null
  tqueue <- newTBQueueIO 128
  tupdates <- newTBQueueIO 128
  pure (State tvalue tqueue tupdates)

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
mainLoop state = go Null
  where
    go val = do
      Put path pvalue <- atomically $ readTBQueue (stateQueue state)
      let newValue = handlePut (Put path pvalue) val
      atomically $ writeTVar (stateValue state) newValue
      atomically $ writeTBQueue (stateUpdates state) (Updated path newValue)
      go newValue
