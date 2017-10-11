module Metrics where

import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Types    as Http
import           Prometheus

type HttpMethodLabel = String
type HttpStatusLabel = String

type HttpRequestCounter = Vector (HttpMethodLabel, HttpStatusLabel) Counter

countHttpRequest :: Http.Method -> Http.Status -> Metric HttpRequestCounter -> IO ()
countHttpRequest method status = withLabel (BS8.unpack method, show $ Http.statusCode status) incCounter

data IcepeakMetrics = IcepeakMetrics
  { icepeakMetricsRequestCounter  :: Metric HttpRequestCounter
  , icepeakMetricsDataSize        :: Metric Gauge
  , icepeakMetricsJournalSize     :: Metric Gauge
  , icepeakMetricsDataWritten     :: Metric Counter
  , icepeakMetricsJournalWritten  :: Metric Counter
  , icepeakMetricsSubscriberCount :: Metric Gauge
  }

createAndRegisterIcepeakMetrics :: IO IcepeakMetrics
createAndRegisterIcepeakMetrics = IcepeakMetrics
  <$> registerIO (vector ("method", "status") requestCounter)
  <*> registerIO (gauge (Info "icepeak_data_size" "Size of data file in bytes."))
  <*> registerIO (gauge (Info "icepeak_journal_size" "Size of journal file in bytes."))
  <*> registerIO (counter (Info "icepeak_data_written" "Total number of bytes written so far."))
  <*> registerIO (counter (Info "icepeak_journal_written" "Total number of bytes written to the journal so far."))
  <*> registerIO (gauge (Info "icepeak_subscriber_count" "Number of websocket subscriber connections."))
  where
    requestCounter = counter (Info "icepeak_http_requests"
                                   "Total number of HTTP requests since starting Icepeak.")

notifyRequest :: Http.Method -> Http.Status -> IcepeakMetrics -> IO ()
notifyRequest method status = countHttpRequest method status . icepeakMetricsRequestCounter

setDataSize :: Real a => a -> IcepeakMetrics -> IO ()
setDataSize val = setGauge (realToFrac val) . icepeakMetricsDataSize

setJournalSize :: Real a => a -> IcepeakMetrics -> IO ()
setJournalSize val = setGauge (realToFrac val) . icepeakMetricsJournalSize

incrementDataWritten :: Real a => a -> IcepeakMetrics -> IO ()
incrementDataWritten val = void . addCounter (realToFrac val) . icepeakMetricsDataWritten

incrementJournalWritten :: Real a => a -> IcepeakMetrics -> IO ()
incrementJournalWritten val = void . addCounter (realToFrac val) . icepeakMetricsJournalWritten

incrementSubscribers :: IcepeakMetrics -> IO ()
incrementSubscribers = incGauge . icepeakMetricsSubscriberCount

decrementSubscribers :: IcepeakMetrics -> IO ()
decrementSubscribers = decGauge . icepeakMetricsSubscriberCount
