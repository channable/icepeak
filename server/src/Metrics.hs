{-# LANGUAGE OverloadedStrings #-}
module Metrics where

import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Prometheus (Counter, Gauge, Info (..), MonadMonitor, Vector, addCounter, counter, decGauge,
                   gauge, incCounter, incGauge, register, setGauge, vector, withLabel)

import qualified Network.HTTP.Types as Http


type HttpMethodLabel = Text
type HttpStatusCode = Text

-- We want to store for each (HTTP method, HTTP status code) pair how many times it has been called
type HttpRequestCounter = Vector (HttpMethodLabel, HttpStatusCode) Counter

countHttpRequest :: MonadMonitor m => Http.Method -> Http.Status -> HttpRequestCounter -> m ()
countHttpRequest method status httpRequestCounter = withLabel httpRequestCounter label incCounter
  where
    label = (textMethod, textStatusCode)
    textMethod = decodeUtf8With lenientDecode method
    textStatusCode = pack $ show $ Http.statusCode status


data IcepeakMetrics = IcepeakMetrics
  { icepeakMetricsRequestCounter    :: HttpRequestCounter
  -- TODO: the following line can be removed after dashboard has been updated to use icepeak_data_size_bytes
  , icepeakMetricsDataSize          :: Gauge
  , icepeakMetricsDataSizeBytes     :: Gauge
  , icepeakMetricsJournalSize       :: Gauge
  , icepeakMetricsDataWritten       :: Counter
  , icepeakMetricsDataWrittenTotal  :: Counter
  , icepeakMetricsJournalWritten    :: Counter
  , icepeakMetricsSubscriberCount   :: Gauge
  }

createAndRegisterIcepeakMetrics :: IO IcepeakMetrics
createAndRegisterIcepeakMetrics = IcepeakMetrics
  <$> register (vector ("method", "status") requestCounter)
  -- TODO: the following line can be removed after dashboard has been updated to use icepeak_data_size_bytes
  <*> register (gauge (Info "icepeak_data_size" "Size of data file in bytes."))
  <*> register (gauge (Info "icepeak_data_size_bytes" "Size of data file in bytes."))
  <*> register (gauge (Info "icepeak_journal_size_bytes"
                            "Size of journal file in bytes."))
  -- TODO: the following line can be removed after dashboard has been updated to use icepeak_data_size_bytes
  <*> register (counter (Info "icepeak_data_written" "Total number of bytes written so far."))
  <*> register (counter (Info "icepeak_data_written_bytes_total" "Total number of bytes written so far."))
  <*> register (counter (Info "icepeak_journal_written_bytes_total"
                              "Total number of bytes written to the journal so far."))
  <*> register (gauge
    (Info "icepeak_subscriber_count" "Number of websocket subscriber connections."))
  where
    requestCounter = counter (Info "icepeak_http_requests"
                                   "Total number of HTTP requests since starting Icepeak.")

notifyRequest :: Http.Method -> Http.Status -> IcepeakMetrics -> IO ()
notifyRequest method status = countHttpRequest method status . icepeakMetricsRequestCounter

setDataSize :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m ()
setDataSize val metrics = do
  -- TODO: the following line can be removed after dashboard has been updated to use icepeak_data_size_bytes
  setGauge (icepeakMetricsDataSize      metrics) (realToFrac val)
  setGauge (icepeakMetricsDataSizeBytes metrics) (realToFrac val)

setJournalSize :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m ()
setJournalSize val metrics = setGauge (icepeakMetricsJournalSize metrics) (realToFrac val)

-- | Increment the total data written to disk by the given number of bytes.
-- Returns True, when it actually increased the counter and otherwise False.
incrementDataWritten :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m Bool
incrementDataWritten num_bytes metrics = do
  -- Ignore the result to silence linter.
  -- TODO: the following line can be removed after dashboard has been updated to use icepeak_data_size_bytes
  _ <- addCounter (icepeakMetricsDataWritten metrics) (realToFrac num_bytes)
  addCounter (icepeakMetricsDataWrittenTotal metrics) (realToFrac num_bytes)

-- | Increment the data written to the journal by the given number of bytes.
-- Returns True, when it actually increased the counter and otherwise False.
incrementJournalWritten :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m Bool
incrementJournalWritten num_bytes metrics = addCounter (icepeakMetricsJournalWritten metrics)
  (realToFrac num_bytes)

incrementSubscribers :: MonadMonitor m => IcepeakMetrics -> m ()
incrementSubscribers = incGauge . icepeakMetricsSubscriberCount

decrementSubscribers :: MonadMonitor m => IcepeakMetrics -> m ()
decrementSubscribers = decGauge . icepeakMetricsSubscriberCount
