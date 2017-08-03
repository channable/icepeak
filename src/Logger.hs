module Logger
(
  LogRecord,
  log,
  processLogRecords
)
where

import Control.Monad (unless)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue, isFullTBQueue)
import Prelude hiding (log)

type LogRecord = String

log :: LogRecord -> TBQueue (Maybe LogRecord) -> IO ()
log record logRecords = atomically $ do
  isFull <- isFullTBQueue logRecords
  unless isFull $ writeTBQueue logRecords (Just record)

processLogRecords :: TBQueue (Maybe LogRecord) -> IO ()
processLogRecords logRecords = go
  where
    go = do
      maybeLogRecord <- atomically $ readTBQueue logRecords
      case maybeLogRecord of
        Just logRecord -> do
          putStrLn $ show logRecord
          go
        -- Stop the loop when we receive a Nothing.
        Nothing -> pure ()
