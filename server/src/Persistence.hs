{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-| This module abstracts over the details of persisting the value. Journaling is
  also handled here, if enabled. -}
module Persistence
  ( PersistentValue
  , PersistenceConfig (..)
  , getValue
  , apply
  , load
  , sync
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Char8      as SBS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Foldable
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Traversable
import           System.Directory           (getFileSize, renameFile)
import           System.IO

import           Logger                     (Logger)
import qualified Logger
import qualified Metrics
import qualified Store

data PersistentValue = PersistentValue
  { pvConfig  :: PersistenceConfig
  , pvValue   :: TVar Store.Value
  , pvIsDirty :: TVar Bool
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
    for_ (pcMetrics . pvConfig $ val) $
      Metrics.incrementJournalWritten (LBS8.length entry)
  -- update value
  atomically $ do
    modifyTVar (pvValue val) (Store.applyModification op)
    writeTVar (pvIsDirty val) True

-- * IO

-- | Load the persisted data from disk and recover journal entries.
load :: PersistenceConfig -> IO (Either String PersistentValue)
load config = runExceptT $ do
  value <- readData (pcDataFile config)
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
sync :: PersistentValue -> IO ()
sync val = do
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
    for_ (pvJournal val) $ \journalHandle ->
      hSetFileSize journalHandle 0
    -- handle metrics last
    forM_ (pcMetrics . pvConfig $ val) $ \m -> do
      size <- getFileSize fileName
      Metrics.setDataSize size m
      Metrics.incrementDataWritten size m

-- * Private helper functions

-- Note that some of these functions are still exported in order to be usable in the test suite

-- | The journal is line-based and therefore consists of a list of strings.
type RawJournalData = [SBS.ByteString]

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
    sync pval

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

-- | Read and decode the data file
readData :: FilePath -> ExceptT String IO Store.Value
readData filePath = ExceptT $ do
  eitherEncodedValue <- try $ withFile filePath ReadMode SBS.hGetContents
  case (eitherEncodedValue :: Either SomeException SBS.ByteString) of
    Left exc -> pure $ Left $ "Failed to read the data from disk: " ++ show exc
    Right encodedValue -> do
      case Aeson.eitherDecodeStrict encodedValue of
        Left msg  -> pure $ Left $ "Failed to decode the initial data: " ++ show msg
        Right value -> pure $ Right $ value

-- | Log a message in the context of a PersistentValue.
logMessage :: PersistentValue -> Text -> IO ()
logMessage pval msg = Logger.postLogBlocking (pcLogger $ pvConfig pval) msg

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
