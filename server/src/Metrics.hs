module Metrics where

import           Prometheus
import qualified Network.HTTP.Types as Http
import qualified Data.ByteString.Char8 as BS8

type HttpMethodLabel = String
type HttpStatusLabel = String

type HttpRequestCounter = Vector (HttpMethodLabel, HttpStatusLabel) Counter

countHttpRequest :: Http.Method -> Http.Status -> Metric HttpRequestCounter -> IO ()
countHttpRequest method status = withLabel (BS8.unpack method, show $ Http.statusCode status) incCounter

data IcepeakMetrics = IcepeakMetrics
  { icepeakMetricsRequestCounter  :: Metric HttpRequestCounter
  , icepeakMetricsDataSize        :: Metric Gauge
  , icepeakMetricsSubscriberCount :: Metric Gauge
  }

createAndRegisterIcepeakMetrics :: IO IcepeakMetrics
createAndRegisterIcepeakMetrics = IcepeakMetrics
  <$> registerIO (vector ("method", "status") requestCounter)
  <*> registerIO (gauge (Info "icepeak_data_size" "Size of data file in bytes."))
  <*> registerIO (gauge (Info "icepeak_subscriber_count" "Number of websocket subscriber connections."))
  where
    requestCounter = counter (Info "icepeak_http_requests"
                                   "Total number of HTTP requests since starting Icepeak.")

notifyRequest :: Http.Method -> Http.Status -> IcepeakMetrics -> IO ()
notifyRequest method status = countHttpRequest method status . icepeakMetricsRequestCounter

setDataSize :: Real a => a -> IcepeakMetrics -> IO ()
setDataSize val = setGauge (realToFrac val) . icepeakMetricsDataSize

incrementSubscribers :: IcepeakMetrics -> IO ()
incrementSubscribers = incGauge . icepeakMetricsSubscriberCount

decrementSubscribers :: IcepeakMetrics -> IO ()
decrementSubscribers = decGauge . icepeakMetricsSubscriberCount
