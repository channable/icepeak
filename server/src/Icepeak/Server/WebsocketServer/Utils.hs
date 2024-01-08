module Icepeak.Server.WebsocketServer.Utils where

import Control.Concurrent.MVar (MVar)
import Data.UUID (UUID)
import Data.Aeson (Value)

import qualified Control.Concurrent.MVar as MVar
import qualified System.Random as Random
import qualified Control.Monad as Monad
import qualified Data.Maybe as Maybe

import Icepeak.Server.Core (Core)

import qualified Icepeak.Server.Core as Core
import qualified Icepeak.Server.Metrics as Metrics

newUUID :: IO UUID
newUUID = Random.randomIO

-- this function is imported by both 'MultiSubscription.hs' and 'SingleSubscription.hs'
-- it is subscription logic that they both have in common
writeToSub :: Core -> MVar Value -> Value -> IO ()
writeToSub core queue val = do
  -- We are the only producer, so either the subscriber already
  -- read the value or we can discard it to replace it with the
  -- new one. We don't need atomicity for this operation.
  -- `tryTakeMVar` basically empties the MVar, from this perspective.
  mbQueue <- MVar.tryTakeMVar queue
  -- If the MVar has not yet been read by the subscriber thread, it means
  -- that the update has been skipped.
  Monad.when (Maybe.isJust mbQueue) $
    Monad.forM_ (Core.coreMetrics core) Metrics.incrementWsSkippedUpdates
  MVar.putMVar queue val
