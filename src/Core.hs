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
  log,
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
import Data.Text (Text)
import Data.UUID (UUID)
import Prelude hiding (log)
import Store (Path)
import Subscription (SubscriptionTree, empty)

import qualified Network.WebSockets as WS
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

type LogRecord = Text

data Core = Core
  { coreCurrentValue :: TVar Value
  , coreQueue :: TBQueue (Maybe Op)
  , coreUpdates :: TBQueue (Maybe Updated)
  , coreClients :: MVar ServerState
  , coreLogRecords :: TBQueue (Maybe LogRecord)
  }

type ServerState = SubscriptionTree UUID WS.Connection

log :: LogRecord -> Core -> IO ()
log record core = atomically $ do
  isFull <- isFullTBQueue (coreLogRecords core)
  unless isFull $ writeTBQueue (coreLogRecords core) (Just record)

newServerState :: ServerState
newServerState = empty

newCore :: IO Core
newCore = do
  tvalue <- newTVarIO Null
  tqueue <- newTBQueueIO 256
  tupdates <- newTBQueueIO 256
  tclients <- newMVar newServerState
  tlogrecords <- newTBQueueIO 256
  pure (Core tvalue tqueue tupdates tclients tlogrecords)

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
          atomically $ writeTVar (coreCurrentValue core) newValue
          atomically $ writeTBQueue (coreUpdates core) (Just $ Updated (opPath op) newValue)
          go newValue
        Nothing -> do
          -- Stop the loop when we receive a Nothing. Tell the update loop to
          -- quit as well.
          atomically $ writeTBQueue (coreUpdates core) Nothing
          pure val
