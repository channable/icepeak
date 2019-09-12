{-# LANGUAGE OverloadedStrings #-}
module MetricsServer where

import           Data.Function                     ((&))
import           Data.Monoid                       ((<>))
import qualified Data.Text                         as Text
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Middleware.Prometheus as PrometheusWai

import           Config                            (MetricsConfig (..))
import           Logger                            (Logger, LogLevel(..), postLog)

metricsServerConfig :: MetricsConfig -> Warp.Settings
metricsServerConfig config = Warp.defaultSettings
  & Warp.setHost (metricsConfigHost config)
  & Warp.setPort (metricsConfigPort config)

runMetricsServer :: Logger -> MetricsConfig -> IO ()
runMetricsServer logger metricsConfig = do
  Logger.postLog logger LogInfo $ "Metrics provided on "
    <> (Text.pack $ show $ metricsConfigHost metricsConfig)
    <> ":"
    <> (Text.pack $ show $ metricsConfigPort metricsConfig)
  Warp.runSettings (metricsServerConfig metricsConfig) PrometheusWai.metricsApp
