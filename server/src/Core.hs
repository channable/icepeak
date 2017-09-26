{-# LANGUAGE OverloadedStrings #-}
module Core
(
  Core (..), -- TODO: Expose only put for clients.
  EnqueueResult (..),
  Op (..),
  ServerState,
  Updated (..),
  enqueueOp,
  getCurrentValue,
  withCoreMetrics,
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
import Control.Monad.IO.Class
import Data.Aeson (Value (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (forM_)
import Data.Text.Lazy.IO (writeFile)
import Data.UUID (UUID)
import Prelude hiding (log, writeFile)
import System.Directory (renameFile)
import qualified System.Posix.Files as Posix

import qualified Network.WebSockets as WS

import Config (Config, configDataFile)
import Logger (LogRecord)
import Store (Path)
import Subscription (SubscriptionTree, empty)

import qualified Store
import qualified Metrics


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
  , coreConfig :: Config
  , coreMetrics :: Maybe Metrics.IcepeakMetrics
  }

type ServerState = SubscriptionTree UUID WS.Connection

newServerState :: ServerState
newServerState = empty

newCore :: Value -> Config -> Maybe Metrics.IcepeakMetrics -> IO Core
newCore initialValue config metrics = do
  tvalue <- newTVarIO initialValue
  tqueue <- newTBQueueIO 256
  tupdates <- newTBQueueIO 256
  tclients <- newMVar newServerState
  tlogrecords <- newTBQueueIO 256
  pure (Core tvalue tqueue tupdates tclients tlogrecords config metrics)

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

withCoreMetrics :: MonadIO m => Core -> (Metrics.IcepeakMetrics -> IO ()) -> m ()
withCoreMetrics core act = liftIO $ forM_ (coreMetrics core) act

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
          let fileName = (configDataFile $ coreConfig core)
              tempFileName = fileName ++ ".new"
          -- we first write to a temporary file here and then do a rename on it
          -- because rename is atomic on Posix and a crash during writing the
          -- temporary file will thus not corrupt the datastore
          writeFile tempFileName (encodeToLazyText newValue)
          renameFile tempFileName fileName
          forM_ (coreMetrics core) $ \m -> do
            stat <- Posix.getFileStatus fileName
            Metrics.setDataSize (Posix.fileSize stat) m
          go newValue
        Nothing -> do
          -- Stop the loop when we receive a Nothing.
          pure val
