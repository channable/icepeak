module Core
(
  Put (..),
  handlePut,
  processPuts,
  processUpdates,
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
data Put = Put [Text] Value deriving (Eq, Show)

-- The main value has been updated at the given path. The payload contains the
-- entire new value. (So not only the inner value at the updated path.)
data Updated = Updated [Text] Value deriving (Eq, Show)

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

-- Drain the queue of put operations and apply them. Once applied, publish the
-- new value as the current one, and also broadcast updates.
processPuts :: Core -> IO Void
processPuts core = go Null
  where
    go val = do
      Put path pvalue <- atomically $ readTBQueue (coreQueue core)
      let newValue = handlePut (Put path pvalue) val
      atomically $ writeTVar (coreCurrentValue core) newValue
      atomically $ writeTBQueue (coreUpdates core) (Updated path newValue)
      go newValue

processUpdates :: Core -> IO Void
processUpdates core = go
  where
    go = do
      Updated path value <- atomically $ readTBQueue (coreUpdates core)
      putStrLn $ "Update at " ++ (show path) ++ ", new value: " ++ (show value)
      go
