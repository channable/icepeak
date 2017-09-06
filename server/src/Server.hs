module Server
(
  runServer,
)
where

import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSockets

runServer :: ServerApp -> Application -> IO ()
runServer wsApp httpApp =
  let
    wsConnectionOpts = WebSockets.defaultConnectionOptions
  in do
    putStrLn "Listening on port 3000."
    Warp.run 3000 $ websocketsOr wsConnectionOpts wsApp httpApp
