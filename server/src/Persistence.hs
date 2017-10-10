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
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString             as SBS
import qualified Data.ByteString.Lazy        as LBS
import           System.Directory            (renameFile)
import           System.IO                   (withFile, Handle, IOMode (..))

import qualified Store

data PersistenceException = PersistenceException String
  deriving (Show)

instance Exception PersistenceException

data PersistentValue = PersistentValue
  { pvConfig  :: PersistenceConfig
  , pvValue   :: TVar Store.Value
  , pvIsDirty :: TVar Bool
  , pvJournal :: Maybe Handle
  }

data PersistenceConfig = PersistenceConfig
  { pcEnableJournaling :: Bool
  , pcDataFile         :: FilePath
  }

-- | Get the actual value
getValue :: PersistentValue -> STM Store.Value
getValue = readTVar . pvValue

-- | Apply a modification, and write it to the journal if enabled.
apply :: Store.Modification -> PersistentValue -> IO ()
apply op val = do
  -- TODO: append to journal if enabled
  atomically $ do
    modifyTVar (pvValue val) (Store.applyModification op)
    writeTVar (pvIsDirty val) True

-- * IO

load :: PersistenceConfig -> IO (Either String PersistentValue)
load config = runExceptT $ do
  value <- readData (pcDataFile config)
  valueVar <- lift $ newTVarIO value
  dirtyVar <- lift $ newTVarIO False
  return PersistentValue
    { pvConfig  = config
    , pvValue   = valueVar
    , pvIsDirty = dirtyVar
    , pvJournal = Nothing
    }

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

-- * Private helper functions
