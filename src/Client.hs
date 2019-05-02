{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Client where

import Control.Lens
import Data.Foldable
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import HookMap (HookMap)
import qualified HookMap
import qualified Bag as Bag
import Bag (Bag)

import Connection
import UI

type Command = Text -> Client -> IO (NextStep, Client)

data Client = Client
  { _clUI       :: !UI
  , _clExts     :: Bag Extension
  , _clConns    :: Map Text IrcConnection
  }

data Extension = Extension
  { _onMessage  :: Client -> Text -> IO Client
  , _onShutdown :: Client -> IO Client
  , _onCommand  :: HookMap Text Command
  }

data NextStep = Continue | Quit | Skip

makeLenses ''Client
makeLenses ''Extension

newExtension :: Extension
newExtension = Extension
  { _onMessage = \cl _ -> return cl
  , _onShutdown = return
  , _onCommand = HookMap.empty
  }

newClient :: Int -> Int -> Client
newClient w h =
  Client
    { _clUI        = emptyUI w h
    , _clExts      = Bag.empty
    , _clConns     = Map.empty
    }

addExtension :: Client -> Extension -> (Bag.Key, Client)
addExtension cl ext = cl & clExts %%~ Bag.insert ext


clientMessage :: Text -> Client -> IO Client
clientMessage msg cl =
  foldlM
    (\cl_ ext -> view onMessage ext cl_ msg)
    cl
    (view clExts cl)

shutdownClient :: Client -> IO ()
shutdownClient cl =
  do -- quit all open connections
     _cl <- foldlM (\cl_ ext -> view onShutdown ext cl_) cl (view clExts cl)
     return ()
