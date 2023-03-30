{-# LANGUAGE OverloadedStrings #-}
module Metrics where

import Control.Monad.IO.Class
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Ratio ((%))
import System.Clock (TimeSpec, diffTimeSpec, toNanoSecs)
import Prometheus (Counter, Gauge, Histogram, Info (..), MonadMonitor, Vector, addCounter, counter, defaultBuckets, exponentialBuckets, decGauge,
                   gauge, histogram, incCounter, incGauge, observe, observeDuration, register, setGauge, vector, withLabel)
import qualified Network.HTTP.Types as Http


type HttpMethodLabel = Text
type HttpStatusCode = Text

-- We want to store for each (HTTP method, HTTP status code) pair how many times it has been called
-- as well as the duration of each reaquest.
type HttpRequestHistogram = Vector (HttpMethodLabel, HttpStatusCode) Histogram

data IcepeakMetrics = IcepeakMetrics
  { icepeakMetricsRequestCounter        :: HttpRequestHistogram
  -- TODO: the following line can be removed after dashboard has been updated to use icepeak_data_size_bytes
  , icepeakMetricsDataSize              :: Gauge
  , icepeakMetricsDataSizeBytes         :: Gauge
  , icepeakMetricsJournalSize           :: Gauge
  , icepeakMetricsDataWritten           :: Counter
  , icepeakMetricsDataWrittenTotal      :: Counter
  , icepeakMetricsJournalWritten        :: Counter
  , icepeakMetricsSubscriberCount       :: Gauge
  , icepeakMetricsQueueAdded            :: Counter
  , icepeakMetricsQueueRemoved          :: Counter
  , icepeakMetricsSyncDuration          :: Histogram
  , icepeakMetricsWsQueueAdded          :: Counter
  , icepeakMetricsWsQueueRemoved        :: Counter
  , icepeakMetricsWsQueueSkippedUpdates :: Counter
  , icepeakMetricsWsSkippedUpdates      :: Counter
  }

createAndRegisterIcepeakMetrics :: IO IcepeakMetrics
createAndRegisterIcepeakMetrics = IcepeakMetrics
  <$> register (vector ("method", "status") requestHistogram)
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
  <*> register (counter (Info "icepeak_internal_queue_items_added"
                              "Total number of items added to the queue."))
  <*> register (counter (Info "icepeak_internal_queue_items_removed"
                              "Total number of items removed from the queue."))
  <*> register (histogram (Info "icepeak_sync_duration" "Duration of a Sync command.")
                          syncBuckets)
  <*> register (counter (Info "icepeak_internal_ws_queue_items_added"
                              "Total number of items added to the WebSocket queue."))
  <*> register (counter (Info "icepeak_internal_ws_queue_items_removed"
                              "Total number of items removed from the WebSocket queue."))
  <*> register (counter (Info "icepeak_internal_ws_queue_skipped_updates"
                              "Total number of updates discarded from the WebSocket queue."))
  <*> register (counter (Info "icepeak_internal_ws_skipped_updates_total"
                              "Total number of updates that have not been sent to subscribers."))
  where
    requestHistogram = histogram (Info "http_request_duration_seconds"
                                     "Duration of HTTP requests since starting Icepeak.")
                               defaultBuckets
    syncBuckets      = exponentialBuckets 0.001 2 12

countHttpRequest :: IcepeakMetrics -> Http.Method -> Http.Status -> TimeSpec -> TimeSpec -> IO ()
countHttpRequest metrics method status start end = withLabel (icepeakMetricsRequestCounter metrics) label (`observe` latency)
  where
    label = (textMethod, textStatus)
    textMethod = decodeUtf8 method
    textStatus = pack $ show (Http.statusCode status)
    latency = fromRational $ toRational (toNanoSecs (end `diffTimeSpec` start) % 1000000000)

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

incrementQueueAdded :: MonadMonitor m => IcepeakMetrics -> m ()
incrementQueueAdded = incCounter . icepeakMetricsQueueAdded

incrementQueueRemoved :: MonadMonitor m => IcepeakMetrics -> m ()
incrementQueueRemoved = incCounter . icepeakMetricsQueueRemoved

measureSyncDuration :: (MonadIO m, MonadMonitor m) => IcepeakMetrics -> m a -> m a
measureSyncDuration = observeDuration . icepeakMetricsSyncDuration

incrementWsQueueAdded :: MonadMonitor m => IcepeakMetrics -> m ()
incrementWsQueueAdded = incCounter . icepeakMetricsWsQueueAdded

incrementWsQueueRemoved :: MonadMonitor m => IcepeakMetrics -> m ()
incrementWsQueueRemoved = incCounter . icepeakMetricsWsQueueRemoved

incrementWsQueueSkippedUpdates :: MonadMonitor m => IcepeakMetrics -> m ()
incrementWsQueueSkippedUpdates = incCounter . icepeakMetricsWsQueueSkippedUpdates

incrementWsSkippedUpdates :: MonadMonitor m => IcepeakMetrics -> m ()
incrementWsSkippedUpdates = incCounter . icepeakMetricsWsSkippedUpdates
