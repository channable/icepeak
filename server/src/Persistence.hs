{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module abstracts over the details of persisting the value. Journaling is
  also handled here, if enabled. -}
module Persistence
  ( PersistentValue
  , PersistenceConfig (..)
  , getValue
  , apply
  , loadFromBackend
  , syncToBackend
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Char8      as SBS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Foldable
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Traversable
import           Database.SQLite.Simple     (FromRow (..), NamedParam (..), Only (..), execute,
                                             execute_, executeNamed, field, open, query_)
import           System.Directory           (getFileSize, renameFile)
import           System.IO
import           System.IO.Error (tryIOError, isDoesNotExistError, isPermissionError)
import           Logger                     (Logger, LogLevel(..))
import qualified Logger
import qualified Metrics
import qualified Store

import Config (StorageBackend (..))

data PersistentValue = PersistentValue
  { pvConfig  :: PersistenceConfig
  , pvValue   :: TVar Store.Value
    -- ^ contains the state of the whole database
  , pvIsDirty :: TVar Bool
    -- ^ flag indicating whether the current value of 'pvValue' has not yet been persisted to disk
  , pvJournal :: Maybe Handle
  }

data PersistenceConfig = PersistenceConfig
  { pcDataFile    :: FilePath
  , pcJournalFile :: Maybe FilePath
  , pcLogger      :: Logger
  , pcMetrics     :: Maybe Metrics.IcepeakMetrics
  }

-- | Get the actual value
getValue :: PersistentValue -> STM Store.Value
getValue = readTVar . pvValue

-- | Apply a modification, and write it to the journal if enabled.
apply :: Store.Modification -> PersistentValue -> IO ()
apply op val = do
  -- append to journal if enabled
  for_ (pvJournal val) $ \journalHandle -> do
    let entry = Aeson.encode op
    LBS8.hPutStrLn journalHandle entry
    for_ (pcMetrics . pvConfig $ val) $ \metrics -> do
      journalPos <- hTell journalHandle
      _ <- Metrics.incrementJournalWritten (LBS8.length entry) metrics
      Metrics.setJournalSize journalPos metrics
  -- update value
  atomically $ do
    modifyTVar (pvValue val) (Store.applyModification op)
    writeTVar (pvIsDirty val) True

-- * IO

loadFromBackend :: StorageBackend -> PersistenceConfig -> IO (Either String PersistentValue)
loadFromBackend File pc = loadFile pc
loadFromBackend Sqlite pc = loadSqliteFile pc

syncToBackend :: StorageBackend -> PersistentValue -> IO ()
syncToBackend File pv = syncFile pv
syncToBackend Sqlite pv = syncSqliteFile pv

-- * SQLite loading and syncing

-- | There is currently just one row and it contains only one column of type SBS.ByteString.
-- This single field holds the whole JSON value for now.
data JsonRow = JsonRow {jsonByteString :: SBS.ByteString} deriving (Show)

instance FromRow JsonRow where
  fromRow = JsonRow <$> field

loadSqliteFile :: PersistenceConfig -> IO (Either String PersistentValue)
loadSqliteFile config = runExceptT $ do
  liftIO $ forM_ (pcMetrics config) $ \m -> do
    size <- getFileSize (pcDataFile config)
    Metrics.setDataSize size m

  value <- readSqliteData (pcDataFile config) (pcLogger config)
  valueVar <- lift $ newTVarIO value
  dirtyVar <- lift $ newTVarIO False
  journal <- for (pcJournalFile config) openJournal
  let val = PersistentValue
        { pvConfig  = config
        , pvValue   = valueVar
        , pvIsDirty = dirtyVar
        , pvJournal = journal
        }
  recoverJournal val
  return val

-- | Read and decode the Sqlite data file from disk
readSqliteData :: FilePath -> Logger -> ExceptT String IO Store.Value
readSqliteData filePath _logger = ExceptT $ do
  -- read the data from SQLite
  conn <- liftIO $ open filePath
  -- TODO: We might want to have a primary key here after all. Then we could have multiple
  -- JSON values next to each other.
  liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS test (value BLOB)"
  jsonRows <- liftIO $ (query_ conn "SELECT * from test" :: IO [JsonRow])

  case jsonRows of
    -- if there is no data yet, we simply return the empty object. We do the same thing for the
    -- file backend.
    [] -> do
      liftIO $ execute conn "INSERT INTO test (value) VALUES (?)" (Only $ Aeson.encode Aeson.emptyObject)
      pure $ Right Aeson.emptyObject
    _  -> case Aeson.eitherDecodeStrict (jsonByteString $ head $ jsonRows) of
            Left msg  -> pure $ Left $ "Failed to decode the initial data: " ++ show msg
            Right value -> pure $ Right (value :: Store.Value)

-- | Write the data to the SQLite file if it has changed.
syncSqliteFile :: PersistentValue -> IO ()
syncSqliteFile val = do
  (dirty, value) <- atomically $ (,) <$> readTVar (pvIsDirty val)
                                     <*> readTVar (pvValue val)
                                     <*  writeTVar (pvIsDirty val) False
  -- simple optimization: only write when something changed
  when dirty $ do
    let filePath = pcDataFile $ pvConfig val

    conn <- open filePath
    -- we can always UPDATE here, since we know that there will be at least one row, since
    -- we issue an INSERT when we load in an empty database
    liftIO $ executeNamed conn "UPDATE test SET value = :value" [":value" := Aeson.encode value]

    -- the journal is idempotent, so there is no harm if icepeak crashes between
    -- the previous and the next action
    for_ (pvJournal val) $ \journalHandle -> do
      hSeek journalHandle AbsoluteSeek 0
      hSetFileSize journalHandle 0
    -- handle metrics last
    forM_ (pcMetrics . pvConfig $ val) $ \m -> do
      size <- getFileSize filePath
      Metrics.setDataSize size m
      Metrics.setJournalSize (0 :: Int) m
      Metrics.incrementDataWritten size m

-- * File loading and syncing

-- | Load the persisted data from disk and recover journal entries.
loadFile :: PersistenceConfig -> IO (Either String PersistentValue)
loadFile config = runExceptT $ do
  value <- readData (pcDataFile config) (pcLogger config)
  liftIO $ forM_ (pcMetrics config) $ \m -> do
    size <- getFileSize (pcDataFile config)
    Metrics.setDataSize size m
  valueVar <- lift $ newTVarIO value
  dirtyVar <- lift $ newTVarIO False
  journal <- for (pcJournalFile config) openJournal
  let val = PersistentValue
        { pvConfig  = config
        , pvValue   = valueVar
        , pvIsDirty = dirtyVar
        , pvJournal = journal
        }
  recoverJournal val
  return val

-- | Write the data to disk if it has changed.
syncFile :: PersistentValue -> IO ()
syncFile val = do
  (dirty, value) <- atomically $ (,) <$> readTVar (pvIsDirty val)
                                     <*> readTVar (pvValue val)
                                     <*  writeTVar (pvIsDirty val) False
  -- simple optimization: only write when something changed
  when dirty $ do
    let fileName = pcDataFile $ pvConfig val
        tempFileName = fileName ++ ".new"
    -- we first write to a temporary file here and then do a rename on it
    -- because rename is atomic on Posix and a crash during writing the
    -- temporary file will thus not corrupt the datastore
    LBS.writeFile tempFileName (Aeson.encode value)
    renameFile tempFileName fileName
    -- the journal is idempotent, so there is no harm if icepeak crashes between
    -- the previous and the next action
    for_ (pvJournal val) $ \journalHandle -> do
      hSeek journalHandle AbsoluteSeek 0
      hSetFileSize journalHandle 0
    -- handle metrics last
    forM_ (pcMetrics . pvConfig $ val) $ \m -> do
      size <- getFileSize fileName
      Metrics.setDataSize size m
      Metrics.setJournalSize (0 :: Int) m
      Metrics.incrementDataWritten size m

-- * Private helper functions

-- Note that some of these functions are still exported in order to be usable in the test suite

-- | Open or create the journal file
openJournal :: FilePath -> ExceptT String IO Handle
openJournal journalFile = ExceptT $ do
  eitherHandle <- try $ do
    h <- openBinaryFile journalFile ReadWriteMode
    hSetBuffering h LineBuffering
    pure h
  case eitherHandle :: Either SomeException Handle of
    Left exc -> pure $ Left $ "Failed to open journal file: " ++ show exc
    Right fileHandle -> pure $ Right fileHandle

-- | Read the modifications from the journal file, apply them and sync again.
-- This should be done when loading the database from disk.
recoverJournal :: PersistentValue -> ExceptT String IO ()
recoverJournal pval = for_ (pvJournal pval) $ \journalHandle -> ExceptT $ fmap formatErr $ try $ do
  initialValue <- atomically $ readTVar (pvValue pval)
  (finalValue, successful, total) <- runRecovery journalHandle initialValue

  when (successful > 0) $ do
    atomically $ do
      writeTVar (pvValue pval) finalValue
      writeTVar (pvIsDirty pval) True
    -- syncing takes care of cleaning the journal
    syncFile pval

  when (total > 0) $ do
    logMessage pval "Journal replayed"
    logMessage pval $ "  failed:     " <> Text.pack (show $ total - successful)
    logMessage pval $ "  successful: " <> Text.pack (show $ successful)

  where
    formatErr :: Either SomeException a -> Either String a
    formatErr (Left exc) = Left $ "Failed to read journal: " ++ show exc
    formatErr (Right x)  = Right x

    runRecovery journalHandle value = do
      -- read modifications from the beginning
      hSeek journalHandle AbsoluteSeek 0
      foldJournalM journalHandle replayLine (value, 0 :: Integer, 0 :: Integer)

    replayLine line (!value, !successful, !total) = do
      when (total == 0) $ do
        logMessage pval "Journal not empty, recovering"
      case Aeson.eitherDecodeStrict line of
        Left err -> do
          let lineNumber = total + 1
          logMessage pval $ failedRecoveryMsg err lineNumber
          pure (value, successful, total + 1)
        Right op -> pure (Store.applyModification op value, successful + 1, total + 1)

    failedRecoveryMsg err line = "Failed to recover journal entry "
      <> Text.pack (show line) <> ": " <> Text.pack err

-- | Read and decode the data file from disk
readData :: FilePath -> Logger -> ExceptT String IO Store.Value
readData filePath logger = ExceptT $ do
  eitherEncodedValue <- tryIOError $ SBS.readFile filePath
  case eitherEncodedValue of
    Left e | isDoesNotExistError e -> do
        -- If there is no icepeak.json file yet, we create an empty one instead.
        let message = "WARNING: Could not read data from " <> Text.pack filePath <>
                      " because the file does not exist yet. Created an empty database instead."

        -- if this fails, we want the whole program to crash since something is wrong
        SBS.writeFile filePath "{}"

        Logger.postLogBlocking logger LogInfo message
        pure $ Right Aeson.emptyObject

    Left e | isPermissionError e -> do
        pure $ Left $ "File " ++ filePath ++ " cannot be read due to a permission error." ++
                      "Please check the file permissions."
    -- other permission errors should also lead to program termination
    Left e -> pure $ Left (show e)

    -- in case the data-file is empty we write the empty object "{}" to it and return it
    Right "" -> do
        let message = "WARNING: The provided --data-file " <> Text.pack filePath <>
                      " is empty. Will write a default database of {} to this file."
        SBS.writeFile filePath "{}"
        Logger.postLogBlocking logger LogInfo message
        pure $ Right Aeson.emptyObject
    Right encodedValue -> do
      case Aeson.eitherDecodeStrict encodedValue of
        Left msg  -> pure $ Left $ "Failed to decode the initial data: " ++ show msg
        Right value -> pure $ Right value

-- | Log a message in the context of a PersistentValue.
logMessage :: PersistentValue -> Text -> IO ()
logMessage pval msg = Logger.postLogBlocking (pcLogger $ pvConfig pval) LogInfo msg

-- | Left fold over all journal entries.
foldJournalM :: Handle -> (SBS8.ByteString -> a -> IO a) -> a -> IO a
foldJournalM h f = go
  where
    go !x = do
      eof <- hIsEOF h
      if eof
        then pure x
        else do
          line <- SBS8.hGetLine h
          x' <- f line x
          go x'
