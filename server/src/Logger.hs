module Logger
(
  Logger,
  LogRecord,
  LogQueue,
  LogLevel(..),
  newLogger,
  postLog,
  postLogBlocking,
  postStop,
  processLogRecords,
  loggerSentryService
)
where

import SentryLogging

import Control.Monad (unless, when)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, readTBQueue, writeTBQueue, isFullTBQueue)
import Data.Text (Text, unpack)
import Data.Maybe (isJust)
import GHC.Natural (Natural)
import Prelude hiding (log)

import qualified System.Log.Raven.Types as Sentry

import qualified Data.Text.IO as T

type LogRecord = Text

data LogLevel = LogInfo | LogError
  deriving (Eq, Ord, Show, Read)

type LogQueue = TBQueue LogCommand

data Logger = Logger { loggerQueue :: LogQueue, loggerSentryService :: Maybe Sentry.SentryService }

data LogCommand = LogRecord LogLevel LogRecord | LogStop
  deriving (Eq, Ord, Show, Read)

newLogger :: Natural -> Bool -> IO Logger
newLogger queueSize disableSentryLogging = Logger
  <$> atomically (newTBQueue queueSize)
  <*> if disableSentryLogging then pure Nothing else getCrashLogger

-- | Post a non-essential log message to the queue. The message is discarded
-- when the queue is full.
postLog :: Logger -> LogLevel -> LogRecord -> IO ()
postLog logger level record = atomically $ do
  isFull <- isFullTBQueue (loggerQueue logger)
  unless isFull $ writeTBQueue (loggerQueue logger) (LogRecord level record)

-- | Post an essential log message to the queue. This function blocks when the
-- queue is full.
postLogBlocking :: Logger -> LogLevel -> LogRecord -> IO ()
postLogBlocking logger level record = atomically $
  writeTBQueue (loggerQueue logger) (LogRecord level record)

postStop :: Logger -> IO ()
postStop logger = atomically $ writeTBQueue (loggerQueue logger) LogStop

processLogRecords :: Logger -> IO ()
processLogRecords logger = go
  where
    go = do
      cmd <- atomically $ readTBQueue (loggerQueue logger)
      case cmd of
        LogRecord logLevel logRecord -> do
          T.putStrLn logRecord
          when (logLevel == LogError && isJust (loggerSentryService logger) ) (
              putStrLn "Sending error message to Sentry" >>
              logCrashMessage "Icepeak error" (loggerSentryService logger) (unpack logRecord)
            )
          go
        -- stop the loop when asked so
        LogStop -> pure ()
