{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebsocketServer where

import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, modifyMVar, readMVar)

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- the path that a client subscribes to, e.g. imports/1/status
type Path = Text

-- a client subscribes to a specific path, for which it will receive updates
type Client = (Path, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

-- TODO: Multiple clients can subscribe to the same path,
-- we need to define identity for a WS.Connection and filter on that
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

-- called for each new client that connects
onConnect :: MVar ServerState -> WS.PendingConnection -> IO ()
onConnect state pending = do
    printRequest pending
    -- accept the connection
    -- TODO: Validate the path and headers of the pending request
    conn <- WS.acceptRequest pending
    -- fork a pinging thread, because browsers...
    WS.forkPingThread conn 30

    -- broadcast ("Number of clients: " `mappend` (T.pack $ show $ length s)) s

    flip finally (onDisconnect ("bla", conn) state) $ do
        acceptMessages conn state

acceptMessages :: WS.Connection -> MVar ServerState -> IO ()
acceptMessages conn state = do
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
        let client = (path, conn)
        _ <- modifyMVar state $ \s ->
          let s' = addClient client s in return (s', s')
        pushUpdates state client

pushUpdates :: MVar ServerState -> Client -> IO ()
pushUpdates state (_path, conn) = forever $ do
    -- TODO: Get the actual updates from Core
    _updates <- readMVar state
    WS.sendTextData conn ("Something changed..." :: Text)

parseMessage :: Text -> Maybe Path
parseMessage msg = case msg of
    _ | prefix `T.isPrefixOf` msg -> Just $ T.drop (T.length prefix) msg
      | otherwise -> Nothing
    where
      prefix = "Path: "

onDisconnect :: (Text, WS.Connection) -> MVar ServerState -> IO ()
onDisconnect client state = do
    putStrLn $ "Disconnected client"
    -- Remove client and return new state
    s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
    broadcast (fst client `mappend` " disconnected") s

-- Print the path and headers of the pending request
printRequest :: WS.PendingConnection -> IO ()
printRequest pending = do
     -- print (WS.pendingRequest pending)
     putStrLn $ show ("\nPath: " <> (WS.requestPath $ WS.pendingRequest pending))
     let headers = WS.requestHeaders $ WS.pendingRequest pending
     T.putStrLn "Headers:"
     forM_ headers print
