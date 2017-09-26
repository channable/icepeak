module MetricsServer where

import           Data.Function                     ((&))
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Middleware.Prometheus as PrometheusWai
import           Text.Printf                       (printf)

import           Config                            (MetricsConfig (..))

metricsServerConfig :: MetricsConfig -> Warp.Settings
metricsServerConfig config = Warp.defaultSettings
  & Warp.setHost (metricsConfigHost config)
  & Warp.setPort (metricsConfigPort config)

runMetricsServer :: MetricsConfig -> IO ()
runMetricsServer metricsConfig = do
  printf "Metrics provided on %s:%d.\n" (show $ metricsConfigHost metricsConfig) (metricsConfigPort metricsConfig)
  Warp.runSettings (metricsServerConfig metricsConfig) PrometheusWai.metricsApp
