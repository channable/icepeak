{-# LANGUAGE OverloadedStrings #-}
module Icepeak.Server.MetricsServer where

import           Data.Function                     ((&))
import qualified Data.Text                         as Text
import qualified Network.HTTP.Types                as Http
import qualified Network.Wai                       as Wai
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Middleware.Prometheus as PrometheusWai

import           Icepeak.Server.Config             (MetricsConfig (..))
import           Icepeak.Server.Logger             (Logger, LogLevel(..), postLog)

metricsServerConfig :: MetricsConfig -> Warp.Settings
metricsServerConfig config = Warp.defaultSettings
  & Warp.setHost (metricsConfigHost config)
  & Warp.setPort (metricsConfigPort config)

-- | WAI application that serves:
--   GET /healthcheck  -> 200 OK
--   everything else   -> Prometheus metrics
metricsApp :: Wai.Application
metricsApp request respond
  | Wai.requestMethod request == Http.methodGet
  , Wai.pathInfo request == ["healthcheck"]
  = respond $ Wai.responseLBS Http.status200 [] ""
  | otherwise
  = PrometheusWai.metricsApp request respond

runMetricsServer :: Logger -> MetricsConfig -> IO ()
runMetricsServer logger metricsConfig = do
  postLog logger LogInfo $ "Metrics and healthcheck provided on "
    <> (Text.pack $ show $ metricsConfigHost metricsConfig)
    <> ":"
    <> (Text.pack $ show $ metricsConfigPort metricsConfig)
    <> " (/metrics, /healthcheck)"
  Warp.runSettings (metricsServerConfig metricsConfig) metricsApp
