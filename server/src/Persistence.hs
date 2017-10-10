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
  , replayModifications
  , parseJournalData
  ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Char8      as SBS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Either                (partitionEithers)
import           Data.Foldable
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Traversable
import           System.Directory           (renameFile)
import           System.IO

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
  , pcLog         :: Text -> IO ()
  }

-- | Get the actual value
getValue :: PersistentValue -> STM Store.Value
getValue = readTVar . pvValue

-- | Apply a modification, and write it to the journal if enabled.
apply :: Store.Modification -> PersistentValue -> IO ()
apply op val = do
  -- append to journal if enabled
  for_ (pvJournal val) $ \journalHandle ->
    LBS8.hPutStrLn journalHandle (Aeson.encode op)
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
    for_ (pvJournal val) $ \journalHandle ->
      hSetFileSize journalHandle 0
    renameFile tempFileName fileName

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
  -- read modifications from the beginning
  journalLines <- do
    hSeek journalHandle AbsoluteSeek 0
    readJournal journalHandle
  -- parse and apply modifications from journal
  let (errs, ops) = parseJournalData journalLines

  when (not $ null ops) $ do
    pcLog (pvConfig pval) "Journal not empty, recovering"

  when (not $ null errs) $ do
    let msg = Text.intercalate "\n" $ "Failed to recover some journal entries:" : map Text.pack errs
    pcLog (pvConfig pval) msg

  atomically $ do
    modifyTVar' (pvValue pval) (replayModifications ops)
    writeTVar (pvIsDirty pval) True
  -- syncing takes care of cleaning the journal
  sync pval

  when (not $ null ops) $ do
    pcLog (pvConfig pval) "Journal replayed"

  where
    formatErr :: Either SomeException a -> Either String a
    formatErr (Left exc) = Left $ "Failed to read journal: " ++ show exc
    formatErr (Right x)  = Right x

-- | Parse journal data in a list of modifications and a list of errors for entries that could not be parsed.
parseJournalData :: RawJournalData -> ([String], [Store.Modification])
parseJournalData = partitionEithers . map Aeson.eitherDecodeStrict

-- | Replay a list of modifications from an initial value.
replayModifications :: [Store.Modification] -> Store.Value -> Store.Value
replayModifications ops initial = foldl' (flip Store.applyModification) initial ops

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

-- | Read the journal file.
readJournal :: Handle -> IO RawJournalData
readJournal h = go where
  go = do
    eof <- hIsEOF h
    if eof
      then pure []
      else (:) <$> SBS8.hGetLine h <*> go
