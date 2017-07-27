{-# LANGUAGE OverloadedStrings #-}
module WebsocketServer where

import qualified Network.WebSockets as WS
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, modifyMVar_, modifyMVar, readMVar)
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

-- type WS.ServerApp = WS.PendingConnection -> IO ()
application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
    -- accept the connection
    -- TODO: Validate the path and headers of the pending request
    conn <- WS.acceptRequest pending
    -- fork a pinging thread, because browsers...
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    -- clients <- readMVar state
    -- TODO: Come up with a wire protocol and validate it
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) -> do
                putStrLn (show $ "Invalid message: " `mappend` msg)
                WS.sendTextData conn ("Wrong announcement" :: Text)
            | otherwise -> flip finally (onDisconnect ("bla", conn) state) $ do
               modifyMVar_ state $ \s -> do
                   let clients' = addClient client s
                   WS.sendTextData conn $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map fst s)
                   broadcast (fst client `mappend` " joined") clients'
                   putStrLn $ "Number of clients: " ++ show (length clients')
                   return clients'
               talk conn state client
          where
            prefix     = "Hi! I am "
            client     = (T.drop (T.length prefix) msg, conn)

onDisconnect :: (Text, WS.Connection) -> MVar ServerState -> IO ()
onDisconnect client state = do
    putStrLn $ "Disconnected client"
    -- Remove client and return new state
    s <- modifyMVar state $ \s ->
        let s' = removeClient client s in return (s', s')
    broadcast (fst client `mappend` " disconnected") s

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
