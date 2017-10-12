{-# LANGUAGE OverloadedStrings #-}
module Core
(
  Core (..), -- TODO: Expose only put for clients.
  EnqueueResult (..),
  Command (..),
  ServerState,
  Updated (..),
  enqueueCommand,
  tryEnqueueCommand,
  getCurrentValue,
  withCoreMetrics,
  lookup,
  newCore,
  postQuit,
  runCommandLoop,
  runSyncTimer
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, putMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue, isFullTBQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class
import Data.Aeson (Value (..))
import Data.Foldable (forM_)
import Data.Traversable (for)
import Data.UUID (UUID)
import Prelude hiding (log, writeFile)

import qualified Network.WebSockets as WS

import Config (Config (..), periodicSyncingEnabled)
import Logger (Logger)
import Store (Path, Modification (..))
import Subscription (SubscriptionTree, empty)
import Persistence (PersistentValue, PersistenceConfig (..))

import qualified Store
import qualified Persistence
import qualified Metrics

-- | Defines the kinds of commands that are handled by the event loop of the Core.
data Command
  = Sync
    -- ^ The @Sync@ command causes the core to write the JSON value to disk.
  | Modify Modification (Maybe (MVar ()))
    -- ^ The @Modify@ command applies a modification (writing or deleting) to the JSON value.
    -- The optional MVar is used to signal that the command has been processed by the core.
  | Stop
    -- ^ The @Stop@ command causes the event loop of the Core to exit.
  deriving (Eq)

-- The main value has been updated at the given path. The payload contains the
-- entire new value. (So not only the inner value at the updated path.)
data Updated = Updated Path Value deriving (Eq, Show)

data EnqueueResult = Enqueued | Dropped

data Core = Core
  { coreCurrentValue :: PersistentValue
  -- the "dirty" flag is set to True whenever the core value has been modified
  -- and is reset to False when it is persisted.
  , coreValueIsDirty :: TVar Bool
  , coreQueue :: TBQueue Command
  , coreUpdates :: TBQueue (Maybe Updated)
  , coreClients :: MVar ServerState
  , coreLogger  :: Logger
  , coreConfig  :: Config
  , coreMetrics :: Maybe Metrics.IcepeakMetrics
  }

type ServerState = SubscriptionTree UUID WS.Connection

newServerState :: ServerState
newServerState = empty

-- | Try to initialize the core. This loads the database and sets up the internal data structures.
newCore :: Config -> Logger -> Maybe Metrics.IcepeakMetrics -> IO (Either String Core)
newCore config logger metrics = do
  let queueCapacity = fromIntegral . configQueueCapacity $ config
  -- load the persistent data from disk
  let filePath = configDataFile config
      journalFile
        | configEnableJournaling config
          && periodicSyncingEnabled config = Just $ filePath ++ ".journal"
        | otherwise = Nothing
  eitherValue <- Persistence.load PersistenceConfig
    { pcDataFile = filePath
    , pcJournalFile = journalFile
    , pcLogger = logger
    , pcMetrics = metrics
    }
  for eitherValue $ \value -> do
    -- create synchronization channels
    tdirty <- newTVarIO False
    tqueue <- newTBQueueIO queueCapacity
    tupdates <- newTBQueueIO queueCapacity
    tclients <- newMVar newServerState
    pure (Core value tdirty tqueue tupdates tclients logger config metrics)

-- Tell the put handler loop and the update handler to quit.
postQuit :: Core -> IO ()
postQuit core = do
  atomically $ do
    writeTBQueue (coreQueue core) Stop
    writeTBQueue (coreUpdates core) Nothing

-- | Try to enqueue a command. It succeeds if the queue is not full, otherwise,
-- nothing is changed. This should be used for non-critical commands that can
-- also be retried later.
tryEnqueueCommand :: Command -> Core -> IO EnqueueResult
tryEnqueueCommand cmd core = atomically $ do
  isFull <- isFullTBQueue (coreQueue core)
  unless isFull $ writeTBQueue (coreQueue core) cmd
  pure $ if isFull then Dropped else Enqueued

-- | Enqueue a command. Blocks if the queue is full. This is used by the sync
-- timer to make sure the sync commands are actually enqueued. In general,
-- whenever it is critical that a command is executed eventually (when reaching
-- the front of the queue), this function should be used.
enqueueCommand :: Command -> Core -> IO ()
enqueueCommand cmd core = atomically $ writeTBQueue (coreQueue core) cmd

getCurrentValue :: Core -> Path -> IO (Maybe Value)
getCurrentValue core path =
  fmap (Store.lookup path) $ atomically $ Persistence.getValue $ coreCurrentValue core

withCoreMetrics :: MonadIO m => Core -> (Metrics.IcepeakMetrics -> IO ()) -> m ()
withCoreMetrics core act = liftIO $ forM_ (coreMetrics core) act

-- | Drain the command queue and execute them. Changes are published to all
-- subscribers. This function returns when executing the 'Stop' command from the
-- queue.
runCommandLoop :: Core -> IO ()
runCommandLoop core = go
  where
    go = do
      command <- atomically $ readTBQueue (coreQueue core)
      case command of
        Modify op maybeNotifyVar -> do
          Persistence.apply op (coreCurrentValue core)
          postUpdate (Store.modificationPath op) core
          -- when periodic syncing is disabled, data is persisted after every modification
          unless (periodicSyncingEnabled $ coreConfig core) $
            Persistence.sync (coreCurrentValue core)
          mapM_ (`putMVar` ()) maybeNotifyVar
          go
        Sync -> do
          Persistence.sync (coreCurrentValue core)
          go
        Stop -> Persistence.sync (coreCurrentValue core)

-- | Post an update to the core's update queue (read by the websocket subscribers)
postUpdate :: Path -> Core -> IO ()
postUpdate path core = atomically $ do
  value <- Persistence.getValue (coreCurrentValue core)
  writeTBQueue (coreUpdates core) (Just $ Updated path value)

-- | Periodically send a 'Sync' command to the 'Core' if enabled in the core
-- configuration.
runSyncTimer :: Core -> IO ()
runSyncTimer core = mapM_ go (configSyncIntervalMicroSeconds $ coreConfig core)
  where
    go interval = forever $ do
      enqueueCommand Sync core
      threadDelay interval
