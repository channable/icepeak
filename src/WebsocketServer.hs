{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebsocketServer where

import Control.Concurrent (MVar, modifyMVar_, yield)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson (Value)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID
import System.Random (randomIO)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

-- the path that a client subscribes to, e.g. imports/1/status
type Path = Text

-- a client subscribes to a specific path, for which it will receive updates
type Client = (UUID, Path, WS.Connection)

type ServerState = [Client]

-- Keeps subscriptions in a tree data structure, so we can efficiently determine
-- which clients need to be notified for a given update.
data SubscriptionTree =
  SubscriptionTree (HashMap UUID WS.Connection) (HashMap Text SubscriptionTree)

emptySubsTree :: SubscriptionTree
emptySubsTree = SubscriptionTree HashMap.empty HashMap.empty

subscribe :: [Text] -> UUID -> WS.Connection -> SubscriptionTree -> SubscriptionTree
subscribe path uuid conn (SubscriptionTree here inner) =
  case path of
    [] -> SubscriptionTree (HashMap.insert uuid conn here) inner
    key : pathTail ->
      let
        subscribeInner = subscribe pathTail uuid conn
        newInner = HashMap.alter (Just . subscribeInner . fromMaybe emptySubsTree) key inner
      in
        SubscriptionTree here newInner

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
        flip finally (onDisconnect client state) $ do
          putStrLn "Adding client"
          modifyMVar_ state (pure . addClient client)
          -- We don't send any messages here; sending is done by the update
          -- loop; it finds the client in the set of subscriptions. But we do
          -- need to keep the thread running, otherwise the connection will be
          -- closed. So we go into an infinite loop here.
          forever yield

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
