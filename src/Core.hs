module Core
(
  Core (coreClients), -- TODO: Expose only put for clients.
  EnqueueResult (..),
  Op (..),
  enqueueOp,
  getCurrentValue,
  handleOp,
  lookup,
  newCore,
  postQuit,
  processOps,
  processUpdates,
)
where

import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue, isFullTBQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar, readTVar)
import Control.Monad (unless)
import Data.Aeson (Value (..))

import qualified Data.HashMap.Strict as HashMap

import Store (Path)
import WebsocketServer (ServerState)

import qualified Store
import qualified WebsocketServer

-- A modification operation.
data Op
  = Put Path Value
  | Delete Path
  deriving (Eq, Show)

opPath :: Op -> Path
opPath op = case op of
  Put path _ -> path
  Delete path -> path

-- The main value has been updated at the given path. The payload contains the
-- entire new value. (So not only the inner value at the updated path.)
data Updated = Updated Path Value deriving (Eq, Show)

data EnqueueResult = Enqueued | Dropped

data Core = Core
  { coreCurrentValue :: TVar Value
  , coreQueue :: TBQueue (Maybe Op)
  , coreUpdates :: TBQueue (Maybe Updated)
  , coreClients :: MVar ServerState
  }

newCore :: IO Core
newCore = do
  tvalue <- newTVarIO Null
  tqueue <- newTBQueueIO 128
  tupdates <- newTBQueueIO 128
  tclients <- newMVar WebsocketServer.newServerState
  pure (Core tvalue tqueue tupdates tclients)

-- Tell the put handler loop and the update handler loop to quit.
postQuit :: Core -> IO ()
postQuit core = atomically $ writeTBQueue (coreQueue core) Nothing

enqueueOp :: Op -> Core -> IO EnqueueResult
enqueueOp op core = atomically $ do
  isFull <- isFullTBQueue (coreQueue core)
  unless isFull $ writeTBQueue (coreQueue core) (Just op)
  pure $ if isFull then Dropped else Enqueued

getCurrentValue :: Core -> Path -> IO (Maybe Value)
getCurrentValue core path =
  fmap (Store.lookup path) $ atomically $ readTVar $ coreCurrentValue core

-- Execute a command.
handleOp :: Op -> Value -> Value
handleOp op value = case op of
  Delete path -> case path of
    [] -> Null -- Deleting the root replaces it with null.
    key : [] -> case value of
      Object dict -> Object $ HashMap.delete key dict
      notObject   -> notObject
    key : pathTail  ->
      let
        deleteInner = handleOp (Delete pathTail)
      in case value of
        Object dict -> Object $ HashMap.adjust deleteInner key dict
        notObject   -> notObject

  Put path newValue -> Store.insert path newValue value

-- Drain the queue of operations and apply them. Once applied, publish the
-- new value as the current one, and also broadcast updates.
processOps :: Core -> IO Value
processOps core = go Null
  where
    go val = do
      maybeOp <- atomically $ readTBQueue (coreQueue core)
      case maybeOp of
        Just op -> do
          let newValue = handleOp op val
          atomically $ writeTVar (coreCurrentValue core) newValue
          atomically $ writeTBQueue (coreUpdates core) (Just $ Updated (opPath op) newValue)
          go newValue
        Nothing -> do
          -- Stop the loop when we receive a Nothing. Tell the update loop to
          -- quit as well.
          atomically $ writeTBQueue (coreUpdates core) Nothing
          pure val

processUpdates :: Core -> IO ()
processUpdates core = go
  where
    go = do
      maybeUpdate <- atomically $ readTBQueue (coreUpdates core)
      case maybeUpdate of
        Just (Updated path value) -> do
          clients <- readMVar (coreClients core)
          WebsocketServer.broadcast path value clients
          putStrLn $ "Update at " ++ (show path) ++ ", new value: " ++ (show value)
          go
        -- Stop the loop when we receive a Nothing.
        Nothing -> pure ()
