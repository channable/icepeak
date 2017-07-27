{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebsocketServer where

import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, modifyMVar)

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

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
    let client = ("bla2", conn)
    -- fork a pinging thread, because browsers...
    WS.forkPingThread conn 30

    -- add the connection to the server state
    s <- modifyMVar state $ \s ->
        let s' = addClient client s in return (s', s')

    broadcast ("Number of clients: " `mappend` (T.pack $ show $ length s)) s

    flip finally (onDisconnect ("bla", conn) state) $ do
        acceptMessages conn

acceptMessages :: WS.Connection -> IO ()
acceptMessages conn = forever $ do
    -- TODO: Come up with a wire protocol and validate it
    msg <- WS.receiveData conn
    T.putStrLn $ "Received message: " <> msg

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
