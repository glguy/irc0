{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Client where

import Control.Concurrent
import Control.Lens
import Data.Foldable
import Data.Map (Map)
import Data.Text (Text)
import Foreign.Ptr
import Foreign.StablePtr
import qualified Data.Map as Map
import qualified Data.Text as Text

import Extension (Extension)
import qualified Extension as Extension
import Connection
import UI

type Command = String -> Client -> IO (NextStep, Client)

data Client = Client
  { _clUI :: !UI
  , _clCommands :: Map Text Command
  , _clStable   :: StablePtr (MVar Client)
  , _clMVar     :: MVar Client
  , _clExts     :: [Extension]
  , _clConns    :: Map Text IrcConnection
  }

data NextStep = Continue | Quit

makeLenses ''Client

newClient :: Int -> Int -> IO Client
newClient w h =
  do mvar <- newEmptyMVar
     stab <- newStablePtr mvar
     return Client
       { _clUI       = emptyUI w h
       , _clCommands = standardCommands
       , _clStable   = stab
       , _clMVar     = mvar
       , _clExts     = []
       , _clConns    = Map.empty
       }

standardCommands :: Map Text Command
standardCommands =
  Map.fromList
    [("load", loadCommand),
     ("conn", connCommand),
     ("exit", exitCommand),
     ("focus", focusCommand)]

exitCommand :: Command
exitCommand _ cl = return (Quit, cl)

focusCommand :: Command
focusCommand name cl =
  return (Continue, cl & clUI . uiFocus ?~ Text.pack name)

connCommand :: Command
connCommand name cl =
  do conn <- newConnection
     let key = Text.pack name
     case cl & clConns . at key <<.~ Just conn of
       (old, cl1) ->
         do for_ old \oldC -> cancelConnection oldC
            return (Continue, cl1 & clUI . uiFocus .~ Just key)


loadCommand :: Command
loadCommand path cl =
  do ext <- Extension.open path
     (cl1, ext') <- parked cl (Extension.startup (castStablePtrToPtr (view clStable cl)) ext)
     return (Continue, over clExts (ext':) cl1)

shutdownExtension :: Client -> Extension -> IO Client
shutdownExtension cl ext = fst <$> parked cl (Extension.shutdown ext)

parked :: Client -> IO b -> IO (Client, b)
parked cl m =
  do putMVar (view clMVar cl) cl
     res <- m
     cl1 <- takeMVar (view clMVar cl)
     return (cl1, res)

reentry :: Ptr () -> (Client -> IO (Client, a)) -> IO a
reentry stab k =
  do mvar <- deRefStablePtr (castPtrToStablePtr stab) :: IO (MVar Client)
     modifyMVar mvar k

clientMessage :: Text -> Client -> IO Client
clientMessage msg cl =
  fmap fst $ parked cl $
  for_ (view clExts cl) \ext ->
    Extension.onMessage ext msg
