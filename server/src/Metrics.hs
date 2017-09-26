module Metrics where

import           Prometheus

data IcepeakMetrics = IcepeakMetrics
  { icepeakMetricsPutCounter      :: Metric Counter
  , icepeakMetricsGetCounter      :: Metric Counter
  , icepeakMetricsDeleteCounter   :: Metric Counter
  , icepeakMetricsDataSize        :: Metric Gauge
  , icepeakMetricsSubscriberCount :: Metric Gauge
  }

createAndRegisterIcepeakMetrics :: IO IcepeakMetrics
createAndRegisterIcepeakMetrics = IcepeakMetrics
  <$> registerIO (counter (Info "put_requests" "Total number of PUT requests since starting Icepeak."))
  <*> registerIO (counter (Info "get_requests" "Total number of GET requests since starting Icepeak."))
  <*> registerIO (counter (Info "delete_requests" "Total number of DELETE requests since starting Icepeak."))
  <*> registerIO (gauge (Info "data_size" "Size of data file in bytes."))
  <*> registerIO (gauge (Info "subscriber_count" "Number of websocket subscriber connections."))

notifyGetRequest :: IcepeakMetrics -> IO ()
notifyGetRequest = incCounter . icepeakMetricsGetCounter

notifyPutRequest :: IcepeakMetrics -> IO ()
notifyPutRequest = incCounter . icepeakMetricsPutCounter

notifyDeleteRequest :: IcepeakMetrics -> IO ()
notifyDeleteRequest = incCounter . icepeakMetricsDeleteCounter

setDataSize :: Real a => a -> IcepeakMetrics -> IO ()
setDataSize val = setGauge (realToFrac val) . icepeakMetricsDataSize

incrementSubscribers :: IcepeakMetrics -> IO ()
incrementSubscribers = incGauge . icepeakMetricsSubscriberCount

decrementSubscribers :: IcepeakMetrics -> IO ()
decrementSubscribers = decGauge . icepeakMetricsSubscriberCount
