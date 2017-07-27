{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebsocketServer where

import Data.Aeson (Value)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID
-- import Control.Exception (finally)
import Control.Monad (forM_)
import Control.Concurrent (MVar, modifyMVar_)
import System.Random (randomIO)

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- the path that a client subscribes to, e.g. imports/1/status
type Path = Text

-- a client subscribes to a specific path, for which it will receive updates
type Client = (UUID, Path, WS.Connection)

type ServerState = [Client]

newUUID :: IO UUID
newUUID = randomIO

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient (uuid, _, _) cs = filter (\(x, _, _) -> uuid /= x) cs

broadcast :: Value -> ServerState -> IO ()
broadcast value clients = do
  forM_ clients $ \(uuid, _, conn) -> do
    putStrLn $ "Sending binary data to " ++ (show uuid)
    WS.sendBinaryData conn (Aeson.encode value)

-- called for each new client that connects
onConnect :: MVar ServerState -> WS.PendingConnection -> IO ()
onConnect state pending = do
    printRequest pending
    -- accept the connection
    -- TODO: Validate the path and headers of the pending request
    conn <- WS.acceptRequest pending
    -- fork a pinging thread, because browsers...
    WS.forkPingThread conn 30

    handleFirstMessage conn state

handleFirstMessage :: WS.Connection -> MVar ServerState -> IO ()
handleFirstMessage conn state = do
    -- TODO: Come up with a wire protocol and validate it
    msg <- WS.receiveData conn
    let parsedMessage = parseMessage msg
    T.putStrLn $ "Received message: " <> msg

    case parsedMessage of
      -- TODO: Disconnect the client
      Nothing -> T.putStrLn $ "Invalid message: " <> msg
      Just path -> do
        T.putStrLn $ "Parsed path: " <> path
        -- add the connection to the server state
        uuid <- newUUID
        let client = (uuid, path, conn)
        --flip finally (onDisconnect client state) $ do
        putStrLn "Adding client"
        modifyMVar_ state (pure . addClient client)
        let
          loop :: IO ()
          loop = loop
        loop

parseMessage :: Text -> Maybe Path
parseMessage msg = case msg of
    _ | prefix `T.isPrefixOf` msg -> Just $ T.drop (T.length prefix) msg
      | otherwise -> Nothing
    where
      prefix = "Path: "

onDisconnect :: Client -> MVar ServerState -> IO ()
onDisconnect client state = do
    putStrLn $ "Disconnected client"
    -- Remove client and return new state
    modifyMVar_ state (pure . removeClient client)

-- Print the path and headers of the pending request
printRequest :: WS.PendingConnection -> IO ()
printRequest pending = do
     -- print (WS.pendingRequest pending)
     putStrLn $ show ("\nPath: " <> (WS.requestPath $ WS.pendingRequest pending))
     let headers = WS.requestHeaders $ WS.pendingRequest pending
     T.putStrLn "Headers:"
     forM_ headers print
