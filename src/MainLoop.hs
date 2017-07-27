module MainLoop (mainLoop) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO)
import Data.Aeson (Value (..))
import Data.Void (Void)
import Data.Text (Text)

-- Put is a command to put a value at a given path.
data Put = Put [Text] Value

data State = State
  { stValue :: TVar Value
  , stQueue :: TBQueue Put
  }

newState :: IO State
newState = do
  tvalue <- newTVarIO Null
  tqueue <- newTBQueueIO 128
  pure (State tvalue tqueue)

mainLoop :: State -> IO Void
mainLoop state =
  mainLoop state
