module CrashLogging where

import Data.Maybe

import Control.Exception
import System.Environment (lookupEnv)


import qualified System.Log.Raven as Sentry
import qualified System.Log.Raven.Transport.HttpConduit as Sentry
import qualified System.Log.Raven.Transport.Debug as Sentry
import qualified System.Log.Raven.Types as Sentry

getCrashLogger :: IO (Maybe Sentry.SentryService)
getCrashLogger = do
    let
    maybeSentryDSN <- pure $ Just "http://fake:fake@fake" --lookupEnv "SENTRY_DSN"
    maybeSentry <- case maybeSentryDSN of
      Nothing -> Nothing <$ putStr "SENTRY_DSN is not set. Won't log to Sentry.\n"
      Just dsn -> Just <$>
        Sentry.initRaven dsn id Sentry.dumpRecord Sentry.errorFallback
    return maybeSentry

logException :: String -> Maybe Sentry.SentryService -> SomeException -> IO ()
logException name mSentryService exception = maybe (pure ()) (\ss -> Sentry.register ss name Sentry.Error (show (exception :: SomeException)) id) mSentryService

logCrashMessage :: String -> Maybe Sentry.SentryService -> String -> IO ()
logCrashMessage name mSentryService message = maybe (pure ()) (\ss -> Sentry.register ss name Sentry.Error message id) mSentryService

runWithCrashLogger :: String -> Sentry.SentryService -> IO () -> IO ()
runWithCrashLogger name sentryService io = handle (
    \exception -> Sentry.register sentryService name Sentry.Error (show (exception :: SomeException)) id
    ) io
