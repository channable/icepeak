module Core
(
  Core (..), -- TODO: Expose only put for clients.
  EnqueueResult (..),
  Op (..),
  ServerState,
  Updated (..),
  enqueueOp,
  getCurrentValue,
  handleOp,
  lookup,
  newCore,
  postQuit,
  processOps
)
where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue, isFullTBQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar, readTVar)
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO (writeFile)
import Data.UUID (UUID)
import Prelude hiding (log, writeFile)
import Store (Path)
import Subscription (SubscriptionTree, empty)

import qualified Network.WebSockets as WS

import Logger (LogRecord)

import qualified Store

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
  , coreLogRecords :: TBQueue (Maybe LogRecord)
  }

type ServerState = SubscriptionTree UUID WS.Connection

newServerState :: ServerState
newServerState = empty

newCore :: Value -> IO Core
newCore value = do
  tvalue <- newTVarIO value
  tqueue <- newTBQueueIO 256
  tupdates <- newTBQueueIO 256
  tclients <- newMVar newServerState
  tlogrecords <- newTBQueueIO 256
  pure (Core tvalue tqueue tupdates tclients tlogrecords)

-- Tell the put handler loop, the update handler and the logger loop to quit.
postQuit :: Core -> IO ()
postQuit core = do
  atomically $ do
    writeTBQueue (coreQueue core) Nothing
    writeTBQueue (coreUpdates core) Nothing
    writeTBQueue (coreLogRecords core) Nothing

enqueueOp :: Op -> Core -> IO EnqueueResult
enqueueOp op core = atomically $ do
  isFull <- isFullTBQueue (coreQueue core)
  unless isFull $ writeTBQueue (coreQueue core) (Just op)
  pure $ if isFull then Dropped else Enqueued

getCurrentValue :: Core -> Path -> IO (Maybe Value)
getCurrentValue core path =
  fmap (Store.lookup path) $ atomically $ readTVar $ coreCurrentValue core

-- Execute an operation.
handleOp :: Op -> Value -> Value
handleOp op value = case op of
  Delete path -> Store.delete path value
  Put path newValue -> Store.insert path newValue value

-- Drain the queue of operations and apply them. Once applied, publish the
-- new value as the current one.
processOps :: Core -> IO Value
processOps core = go Null
  where
    go val = do
      maybeOp <- atomically $ readTBQueue (coreQueue core)
      case maybeOp of
        Just op -> do
          let newValue = handleOp op val
          atomically $ do
            writeTVar (coreCurrentValue core) newValue
            writeTBQueue (coreUpdates core) (Just $ Updated (opPath op) newValue)
          -- persist the updated Json object to disk
          -- TODO: make it configurable how often we do this (like in Redis)
          putStrLn $ "Applying operation: " ++ show op
          putStrLn $ "newValue: " ++ show newValue
          writeFile "icepeak.json" (encodeToLazyText newValue)
          go newValue
        Nothing -> do
          -- Stop the loop when we receive a Nothing.
          pure val
