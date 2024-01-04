module Icepeak.Server.WebsocketServer.Utils where

import Data.UUID (UUID)

import qualified System.Random as Random

newUUID :: IO UUID
newUUID = Random.randomIO
