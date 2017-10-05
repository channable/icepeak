{-# LANGUAGE OverloadedStrings #-}
module Core
(
  Core (..), -- TODO: Expose only put for clients.
  EnqueueResult (..),
  Command (..),
  Modification (..),
  ServerState,
  Updated (..),
  enqueueCommand,
  tryEnqueueCommand,
  getCurrentValue,
  withCoreMetrics,
  applyModification,
  lookup,
  newCore,
  postQuit,
  runCommandLoop,
  runSyncTimer
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue, isFullTBQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, writeTVar, readTVar)
import Control.Exception (try, SomeException)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class
import Data.Aeson (Value (..), encode, eitherDecodeStrict)
import qualified Data.ByteString as SBS
import Data.ByteString.Lazy (writeFile)
import Data.Foldable (forM_)
import Data.Maybe (isNothing)
import Data.Traversable (for)
import Data.UUID (UUID)
import Prelude hiding (log, writeFile)
import System.Directory (renameFile)
import System.IO (withFile, IOMode (..))
import qualified System.Posix.Files as Posix

import qualified Network.WebSockets as WS

import Config (Config, configDataFile, configQueueCapacity, configSyncIntervalMicroSeconds)
import Logger (LogRecord)
import Store (Path)
import Subscription (SubscriptionTree, empty)

import qualified Store
import qualified Metrics


-- A modification operation.
data Modification
  = Put Path Value
  | Delete Path
  deriving (Eq, Show)

data Command
  = Sync
  | Modify Modification
  | Stop
  deriving (Eq, Show)

modificationPath :: Modification -> Path
modificationPath op = case op of
  Put path _ -> path
  Delete path -> path

-- The main value has been updated at the given path. The payload contains the
-- entire new value. (So not only the inner value at the updated path.)
data Updated = Updated Path Value deriving (Eq, Show)

data EnqueueResult = Enqueued | Dropped

data Core = Core
  { coreCurrentValue :: TVar Value
  , coreValueIsDirty :: TVar Bool
  , coreQueue :: TBQueue Command
  , coreUpdates :: TBQueue (Maybe Updated)
  , coreClients :: MVar ServerState
  , coreLogRecords :: TBQueue (Maybe LogRecord)
  , coreConfig :: Config
  , coreMetrics :: Maybe Metrics.IcepeakMetrics
  }

type ServerState = SubscriptionTree UUID WS.Connection

newServerState :: ServerState
newServerState = empty

-- | Try to initialize the core. This loads the database and sets up the internal data structures.
newCore :: Config -> Maybe Metrics.IcepeakMetrics -> IO (Either String Core)
newCore config metrics = do
  let queueCapacity = fromIntegral . configQueueCapacity $ config
  -- load the persistent data from disk
  let filePath = configDataFile config
  eitherValue <- readData filePath metrics
  for eitherValue $ \initialValue -> do
    -- create synchronization channels
    tvalue <- newTVarIO initialValue
    tdirty <- newTVarIO False
    tqueue <- newTBQueueIO queueCapacity
    tupdates <- newTBQueueIO queueCapacity
    tclients <- newMVar newServerState
    tlogrecords <- newTBQueueIO queueCapacity
    pure (Core tvalue tdirty tqueue tupdates tclients tlogrecords config metrics)

-- Tell the put handler loop, the update handler and the logger loop to quit.
postQuit :: Core -> IO ()
postQuit core = do
  atomically $ do
    writeTBQueue (coreQueue core) Stop
    writeTBQueue (coreUpdates core) Nothing
    writeTBQueue (coreLogRecords core) Nothing

-- | Try to enqueue a command. It succeeds if the queue is not full, otherwise,
-- nothing is changed.
tryEnqueueCommand :: Command -> Core -> IO EnqueueResult
tryEnqueueCommand op core = atomically $ do
  isFull <- isFullTBQueue (coreQueue core)
  unless isFull $ writeTBQueue (coreQueue core) op
  pure $ if isFull then Dropped else Enqueued

-- | Enqueue a command. Blocks if the queue is full.
enqueueCommand :: Command -> Core -> IO ()
enqueueCommand cmd core = atomically $ writeTBQueue (coreQueue core) cmd

getCurrentValue :: Core -> Path -> IO (Maybe Value)
getCurrentValue core path =
  fmap (Store.lookup path) $ atomically $ readTVar $ coreCurrentValue core

withCoreMetrics :: MonadIO m => Core -> (Metrics.IcepeakMetrics -> IO ()) -> m ()
withCoreMetrics core act = liftIO $ forM_ (coreMetrics core) act

-- Execute a modification.
applyModification :: Modification -> Value -> Value
applyModification op value = case op of
  Delete path -> Store.delete path value
  Put path newValue -> Store.insert path newValue value

-- | Drain the command queue and execute them. Changes are published to all
-- subscribers. This function returns when executing the 'Stop' command from the
-- queue.
runCommandLoop :: Core -> IO Value
runCommandLoop core = atomically (readTVar (coreCurrentValue core)) >>= go
  where
    go val = do
      command <- atomically $ readTBQueue (coreQueue core)
      case command of
        Modify op -> do
          let newValue = applyModification op val
          atomically $ do
            writeTVar (coreCurrentValue core) newValue
            writeTVar (coreValueIsDirty core) True
            writeTBQueue (coreUpdates core) (Just $ Updated (modificationPath op) newValue)
          when (isNothing $ configSyncIntervalMicroSeconds $ coreConfig core) $
            persistData core
          go newValue
        Sync -> do
          persistData core
          go val
        Stop -> do
          persistData core
          pure val

-- | Periodically send a 'Sync' command to the 'Core' if enabled in the core
-- configuration.
runSyncTimer :: Core -> IO ()
runSyncTimer core = mapM_ go (configSyncIntervalMicroSeconds $ coreConfig core)
  where
    go interval = forever $ do
      enqueueCommand Sync core
      threadDelay interval

persistData :: Core -> IO ()
persistData core = do
  (dirty, value) <- atomically $ (,) <$> readTVar (coreValueIsDirty core)
                                     <*> readTVar (coreCurrentValue core)
                                     <*  writeTVar (coreValueIsDirty core) False
  when dirty $ do
    -- persist the updated Json object to disk
    let fileName = (configDataFile $ coreConfig core)
        tempFileName = fileName ++ ".new"
    -- we first write to a temporary file here and then do a rename on it
    -- because rename is atomic on Posix and a crash during writing the
    -- temporary file will thus not corrupt the datastore
    writeFile tempFileName (encode value)
    renameFile tempFileName fileName
    forM_ (coreMetrics core) $ \m -> do
      stat <- Posix.getFileStatus fileName
      Metrics.setDataSize (Posix.fileSize stat) m
      Metrics.incrementDataWritten (Posix.fileSize stat) m

readData :: FilePath -> Maybe Metrics.IcepeakMetrics -> IO (Either String Value)
readData filePath metrics = do
  eitherEncodedValue <- try $ withFile filePath ReadMode SBS.hGetContents
  case (eitherEncodedValue :: Either SomeException SBS.ByteString) of
    Left exc -> pure $ Left $ "Failed to read the data from disk: " ++ show exc
    Right encodedValue -> do
      forM_ metrics $ Metrics.setDataSize (SBS.length encodedValue)
      case eitherDecodeStrict encodedValue of
        Left msg  -> pure $ Left $ "Failed to decode the initial data: " ++ show msg
        Right value -> pure $ Right $ value
