{-# Language BlockArguments, OverloadedStrings, TemplateHaskell #-}
module Client where

import           Control.Exception
import           Control.Lens
import           Data.Foldable
import           Data.Map       (Map)
import           Data.Text      (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Irc.RawIrcMsg as Irc

import           Bag            (Bag)
import           Connection
import           HookMap        (HookMap)
import           UI
import qualified Bag as Bag
import qualified HookMap

type Command = Text -> Client -> IO (NextStep, Client)

data Client = Client
  { _clUI       :: !UI
  , _clExts     :: Bag Extension
  , _clConns    :: Map Text IrcConnection
  }

data Extension = Extension
  { _onMessage  :: Client -> Text -> Irc.RawIrcMsg -> IO (NextStep, Client)
  , _onShutdown :: Client -> IO Client
  , _onCommand  :: HookMap Text Command
  }

data NextStep = Continue | Quit | Skip

makeLenses ''Client
makeLenses ''Extension

newExtension :: Extension
newExtension = Extension
  { _onMessage = \cl _ _ -> return (Continue, cl)
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

addLine :: Text -> Client -> Client
addLine x cl = cl & clUI . uiLines %~ cons x

clientMessage :: Text -> Irc.RawIrcMsg -> Client -> IO (NextStep, Client)
clientMessage net msg cl =
  runHandlers
    [ \cl_ -> view onMessage ext cl_ net msg | ext <- toList (view clExts cl) ]
    cl

shutdownClient :: Client -> IO ()
shutdownClient cl =
  do -- quit all open connections
     _cl <-
       foldlM
         (\cl_ ext -> wrapSingle (view onShutdown ext) cl_)
         cl
         (view clExts cl)
     return ()

removeExtension :: Bag.Key -> Client -> IO Client
removeExtension i cl =
  case preview (clExts . ix i) cl of
    Nothing -> return cl
    Just ext ->
      do cl' <- wrapSingle (view onShutdown ext) cl
         return (cl' & clExts %~ Bag.delete i)

runHandlers :: [Client -> IO (NextStep, Client)] -> Client -> IO (NextStep, Client)
runHandlers [] cl = return (Continue, cl)
runHandlers (h:hs) cl =
  do (next, cl') <- wrapHandler h cl
     case next of
       Continue -> runHandlers hs cl'
       Quit     -> return (Quit, cl')
       Skip     -> return (Skip, cl')

wrapSingle ::
  (Client -> IO Client) ->
  (Client -> IO Client)
wrapSingle h cl =
  do res <- try (h cl) :: IO (Either SomeException Client)
     return $! case res of
       Left e ->
         addLine (Text.pack (displayException (e :: SomeException))) cl
       Right cl' -> cl'

wrapHandler ::
  (Client -> IO (NextStep, Client)) ->
  (Client -> IO (NextStep, Client))
wrapHandler h cl =
  do res <- try (h cl)
     return $! case res of
       Right r -> r
       Left e ->
         (Continue, addLine (Text.pack (displayException (e :: SomeException))) cl)
