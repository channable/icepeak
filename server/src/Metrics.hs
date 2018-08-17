{-# LANGUAGE OverloadedStrings #-}
module Metrics where

import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import qualified Network.HTTP.Types as Http

import Prometheus

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
  { icepeakMetricsRequestCounter  :: HttpRequestCounter
  , icepeakMetricsDataSize        :: Gauge
  , icepeakMetricsJournalSize     :: Gauge
  , icepeakMetricsDataWritten     :: Counter
  , icepeakMetricsJournalWritten  :: Counter
  , icepeakMetricsSubscriberCount :: Gauge
  }

createAndRegisterIcepeakMetrics :: IO IcepeakMetrics
createAndRegisterIcepeakMetrics = IcepeakMetrics
  <$> register (vector ("method", "status") requestCounter)
  <*> register (gauge (Info "icepeak_data_size" "Size of data file in bytes."))
  <*> register (gauge (Info "icepeak_journal_size_bytes"
                            "Size of journal file in bytes."))
  <*> register (counter (Info "icepeak_data_written" "Total number of bytes written so far."))
  <*> register (counter (Info "icepeak_journal_written_bytes_total"
                              "Total number of bytes written to the journal so far."))
  <*> register (gauge (Info "icepeak_subscriber_count" "Number of websocket subscriber connections."))
  where
    requestCounter = counter (Info "icepeak_http_requests"
                                   "Total number of HTTP requests since starting Icepeak.")

notifyRequest :: Http.Method -> Http.Status -> IcepeakMetrics -> IO ()
notifyRequest method status = countHttpRequest method status . icepeakMetricsRequestCounter

setDataSize :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m ()
setDataSize val metrics = setGauge (icepeakMetricsDataSize metrics) (realToFrac val)

setJournalSize :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m ()
setJournalSize val metrics = setGauge (icepeakMetricsJournalSize metrics) (realToFrac val)

incrementDataWritten :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m Bool
incrementDataWritten val metrics = addCounter (icepeakMetricsDataWritten metrics) (realToFrac val)

incrementJournalWritten :: (MonadMonitor m, Real a) => a -> IcepeakMetrics -> m Bool
incrementJournalWritten val metrics = addCounter (icepeakMetricsJournalWritten metrics) (realToFrac val)

incrementSubscribers :: MonadMonitor m => IcepeakMetrics -> m ()
incrementSubscribers = incGauge . icepeakMetricsSubscriberCount

decrementSubscribers :: MonadMonitor m => IcepeakMetrics -> m ()
decrementSubscribers = decGauge . icepeakMetricsSubscriberCount
