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

application :: MVar ServerState -> WS.ServerApp


-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.
--
-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

-- When a client is succesfully connected, we read the first message. This should
-- be in the format of "Hi! I am Jasper", where Jasper is the requested username.

    msg <- WS.receiveData conn
    -- clients <- readMVar state
    case msg of

-- Check that the first message has the right format:

        _   | not (prefix `T.isPrefixOf` msg) -> do
                putStrLn (show $ "Invalid message: " `mappend` msg)
                WS.sendTextData conn ("Wrong announcement" :: Text)

-- All is right! We're going to allow the client, but for safety reasons we *first*
-- setup a `disconnect` function that will be run when the connection is closed.

            | otherwise -> flip finally disconnect $ do

-- We send a "Welcome!", according to our own little protocol. We add the client to
-- the list and broadcast the fact that he has joined. Then, we give control to the
-- 'talk' function.

               modifyMVar_ state $ \s -> do
                   let s' = addClient client s
                   WS.sendTextData conn $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map fst s)
                   broadcast (fst client `mappend` " joined") s'
                   return s'
               talk conn state client
          where
            prefix     = "Hi! I am "
            client     = (T.drop (T.length prefix) msg, conn)
            disconnect = do
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
