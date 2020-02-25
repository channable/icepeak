{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-| This module abstracts over the details of persisting the value. Journaling is
  also handled here, if enabled. -}
module Persistence
  ( PersistentValue
  , PersistenceConfig (..)
  , getDataFile
  , getValue
  , apply
  , loadFromBackend
  , setupStorageBackend
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
-- import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Traversable
import           Database.SQLite.Simple     (FromRow (..), NamedParam (..), Only (..), execute,
                                             execute_, executeNamed, field, open, query_)
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import qualified Hasql.Statement as Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import           System.Directory           (getFileSize, renameFile)
import           System.Exit                (die)
import           System.IO
import           System.IO.Error (tryIOError, isDoesNotExistError, isPermissionError)
import           Logger                     (Logger, LogLevel(..))
import qualified Logger
import qualified Metrics
import qualified Store

import GHC.Int (Int64)

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


-- If no --data-file was supplied we default to either icepeak.json, for the file backend,
-- or icepeak.db, for the Sqlite backend
getDataFile :: StorageBackend -> Maybe FilePath -> FilePath
getDataFile _ (Just filePath) = filePath
getDataFile File _ = "icepeak.json"
getDataFile Sqlite _ = "icepeak.db"
getDataFile Postgres _ = "postgres.db"  -- TODO: This doesn't make any sense for Postgres

-- * IO

-- | Ensure that we can access the chosen storage file, and validate that it has the right file format
setupStorageBackend :: StorageBackend -> FilePath -> IO ()
setupStorageBackend File filePath = do
  eitherEncodedValue <- tryIOError $ SBS.readFile filePath
  case eitherEncodedValue of
    Left e | isDoesNotExistError e -> do
        -- If there is no icepeak.json file yet, we create an empty one instead.
        let message = "WARNING: Could not read data from " ++ filePath ++
                      " because the file does not exist yet. Created an empty database instead."

        -- if this fails, we want the whole program to crash since something is wrong
        SBS.writeFile filePath "{}"
        putStrLn message

    Left e | isPermissionError e ->
        die $ "File " ++ filePath ++ " cannot be read due to a permission error. Please check the file permissions."
    -- other errors should also lead to program termination
    Left e -> die (show e)

    -- in case the data-file is empty we write the empty object "{}" to it and return it
    Right "" -> do
        let message = "WARNING: The provided --data-file " ++ filePath ++
                      " is empty. Will write a default database of {} to this file."
        putStrLn message
        SBS.writeFile filePath "{}"
    Right encodedValue -> case Aeson.eitherDecodeStrict encodedValue of
        Left msg  -> die $ "Failed to decode the initial data in " ++ filePath ++ ": " ++ show msg
        Right (_value :: Aeson.Value) -> pure ()
setupStorageBackend Sqlite filePath = do
  -- read the data from SQLite
  conn <- liftIO $ open filePath
  liftIO $ execute_ conn "CREATE TABLE IF NOT EXISTS icepeak (value BLOB)"

  jsonRows <- liftIO $ (query_ conn "SELECT * FROM icepeak" :: IO [JsonRow])
  case jsonRows of
    -- ensure that there is one row in the table, so that we can UPDATE it later
    [] -> liftIO $ execute conn "INSERT INTO icepeak (value) VALUES (?)" (Only $ Aeson.encode Aeson.emptyObject)
    _ -> pure ()

-- TODO: The "filePath" could be the connection string, but that would be a bit of a hack.
-- We ensure here that we can connect to Postgres as the right user, and that the "icepeak" table exists and
-- contains exactly one row (with one column, called 'value')
setupStorageBackend Postgres _filePath =
  bracket
    (Connection.acquire connectionSettings)
    (\connectionResult -> either (error . show) Connection.release connectionResult)
    setupIcepeakTable

  where
    setupIcepeakTable :: Either Connection.ConnectionError Connection.Connection -> IO ()
    setupIcepeakTable connectionResult =
      case connectionResult of
        Left (Just errMsg) -> error $ "Could not connect to Postgres: " ++  show errMsg
        Left Nothing -> error "Unspecified Postgres connection error"
        Right connection -> do
          jsonRows <- Session.run (createSession >> selectSession) connection
          case jsonRows of
            Left queryError -> error $ "Could not query the icepeak table: " ++  show queryError
            Right nrRows -> case nrRows of
              0 -> Session.run insertSession connection >>= either (error . show) (\_ -> pure ())
              1 -> pure ()  -- there should be exactly one row
              _ -> error "There should not be more than one row in the icepeak table. Please fix manually."

    -- TODO: Read these settings from environment variables
    connectionSettings = Connection.settings "localhost" 5434 "icepeak" "icepeak" "icepeak"

    createSession :: Session.Session  ()
    createSession = Session.statement () createTable

    selectSession :: Session.Session Int64
    selectSession = Session.statement () selectStar

    insertSession :: Session.Session ()
    insertSession = Session.statement () insertJsonValue

    createTable :: Statement.Statement () ()
    createTable =  Statement.Statement
      "CREATE TABLE IF NOT EXISTS icepeak (value JSONB)"
      Encoders.noParams
      Decoders.noResult
      False -- don't prepare this statement, since we are running it only once

    selectStar :: Statement.Statement () Int64
    selectStar = Statement.Statement
      "SELECT count(*) FROM icepeak"
      Encoders.noParams
      (Decoders.singleRow $ Decoders.column (Decoders.nonNullable Decoders.int8))
      False -- don't prepare this statement, since we are running it only once

    insertJsonValue :: Statement.Statement () ()
    insertJsonValue = Statement.Statement
      "INSERT INTO icepeak (value) VALUES ('{}'::jsonb);"
      Encoders.noParams
      Decoders.noResult
      False -- don't prepare this statement, since we are running it only once

loadFromBackend :: StorageBackend -> PersistenceConfig -> IO (Either String PersistentValue)
loadFromBackend backend config = runExceptT $ do
  let metrics = pcMetrics config
      dataFilePath = pcDataFile config

  -- We immediately set the dataSize metric, so that Prometheus can start scraping it
  liftIO $ forM_ metrics $ \metric -> do
    size <- getFileSize dataFilePath
    Metrics.setDataSize size metric

  -- Read the data from disk and parse it as an Aeson.Value
  value <- case backend of
    File -> readData dataFilePath
    Sqlite -> readSqliteData dataFilePath
    Postgres -> readPostgresData dataFilePath

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

syncToBackend :: StorageBackend -> PersistentValue -> IO ()
syncToBackend File pv = syncFile pv
syncToBackend Sqlite pv = syncSqliteFile pv
syncToBackend Postgres pv = syncPostgres pv

-- * SQLite loading and syncing

-- | There is currently just one row and it contains only one column of type SBS.ByteString.
-- This single field holds the whole JSON value for now.
data JsonRow = JsonRow {jsonByteString :: SBS.ByteString} deriving (Show)

instance FromRow JsonRow where
  fromRow = JsonRow <$> field

readPostgresData :: FilePath -> ExceptT String IO Store.Value
readPostgresData _filePath = undefined

-- | Read and decode the Sqlite data file from disk
readSqliteData :: FilePath -> ExceptT String IO Store.Value
readSqliteData filePath = ExceptT $ do
  -- read the data from SQLite
  conn <- liftIO $ open filePath
  jsonRows <- liftIO $ (query_ conn "SELECT * from icepeak" :: IO [JsonRow])

  case jsonRows of
    -- the 'setupStorageBackend' function verifies that we can read the database and that at least one row exists
    [] -> pure $ Right Aeson.emptyObject
    _  -> case Aeson.eitherDecodeStrict (jsonByteString $ head $ jsonRows) of
            Left msg  -> pure $ Left $ "Failed to decode the initial data: " ++ show msg
            Right value -> pure $ Right (value :: Store.Value)

syncPostgres :: PersistentValue -> IO ()
syncPostgres _val = undefined

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
    liftIO $ executeNamed conn "UPDATE icepeak SET value = :value" [":value" := Aeson.encode value]

    -- the journal is idempotent, so there is no harm if icepeak crashes between
    -- the previous and the next action
    truncateJournal val

    -- handle metrics last
    updateMetrics val

-- * File syncing

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
    truncateJournal val

    -- handle metrics last
    updateMetrics val

-- * Private helper functions

-- Note that some of these functions are still exported in order to be usable in the test suite

-- | Seek to the beginning of the journal file and set the file size to zero.
-- This should be called after all journal entries have been replayed, and the data has been
-- synced to disk.
truncateJournal :: PersistentValue -> IO ()
truncateJournal val =
    for_ (pvJournal val) $ \journalHandle -> do
      -- we must seek back to the beginning of the file *before* calling hSetFileSize, since that
      -- function does not change the file cursor, which means that the first write that follows
      -- would fill up the file with \NUL bytes up to the original cursor position.
      hSeek journalHandle AbsoluteSeek 0
      hSetFileSize journalHandle 0

-- | We keep track of three metrics related to persistence:
-- 1. The current size of the data file
-- 2. The current size of the journal file
-- 3. The total amount of data written to disk since the process was started
--    (not counting journal writes)
updateMetrics :: PersistentValue -> IO ()
updateMetrics val = do
    let filePath = pcDataFile . pvConfig $ val
        metrics = pcMetrics . pvConfig $ val
    forM_ metrics $ \metric -> do
      size <- getFileSize filePath
      Metrics.setDataSize size metric
      Metrics.setJournalSize (0 :: Int) metric
      Metrics.incrementDataWritten size metric

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
readData :: FilePath -> ExceptT String IO Store.Value
readData filePath = ExceptT $ do
  eitherEncodedValue <- tryIOError $ SBS.readFile filePath
  case eitherEncodedValue of
    -- we do not expect any errors here, since we validated the file earlier already
    Left e -> pure $ Left (show e)
    Right encodedValue -> case Aeson.eitherDecodeStrict encodedValue of
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
