module MainLoop
(
  Put (..),
  handlePut,
  mainLoop,
  newCore,
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

data Core = Core
  { coreCurrentValue :: TVar Value
  , coreQueue :: TBQueue Put
  , coreUpdates :: TBQueue Updated
  }

newCore :: IO Core
newCore = do
  tvalue <- newTVarIO Null
  tqueue <- newTBQueueIO 128
  tupdates <- newTBQueueIO 128
  pure (Core tvalue tqueue tupdates)

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

mainLoop :: Core -> IO Void
mainLoop core = go Null
  where
    go val = do
      Put path pvalue <- atomically $ readTBQueue (coreQueue core)
      let newValue = handlePut (Put path pvalue) val
      atomically $ writeTVar (coreCurrentValue core) newValue
      atomically $ writeTBQueue (coreUpdates core) (Updated path newValue)
      go newValue
