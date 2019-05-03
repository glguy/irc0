{-# Language OverloadedStrings, CPP, BlockArguments #-}
module Extension.CApi
  ( reentry
  , capiExtension

  -- just to avoid the warnings
  , ircclient_print
  , ircclient_send
  , ircclient_unhook
  , ircclient_hook_command
  ) where

import Foreign
import Foreign.C
import System.Posix.DynamicLinker
import Control.Concurrent
import Data.IORef
import Data.Text (Text)
import Control.Lens
import qualified Data.Text.Foreign as Text
import qualified Data.Text as Text

import Client
import Connection (sendConnection)
import UI (uiLines)
import qualified HookMap
import qualified Bag
import Bag (Bag)
import WithIO

capiExtension :: Extension
capiExtension = newExtension & onCommand %~
  snd . HookMap.insert 0 "load" loadCommand

type Wrapper a = FunPtr a -> a
type StartupFun = Ptr () -> IO (Ptr ())
type MessageFun = Ptr () -> CString -> CSize -> IO CInt
type ShutdownFun = Ptr () -> IO ()

data HookEntry
  = CommandHook !Int (Ptr ())

data CExtension = CExtension
  { extIdRef   :: IORef Bag.Key
  , hooksRef   :: IORef (Bag HookEntry)
  , userRef    :: IORef (Ptr ())
  , clientMVar :: !(MVar Client)
  }

foreign import ccall "dynamic" dynStartup   :: Wrapper StartupFun
foreign import ccall "dynamic" dynMessage   :: Wrapper MessageFun
foreign import ccall "dynamic" dynShutdown  :: Wrapper ShutdownFun
foreign import ccall "dynamic" dynCommandCb :: Wrapper CommandCb

#ifndef GHCI
foreign export ccall ircclient_print            :: Print
foreign export ccall ircclient_send             :: Send
foreign export ccall ircclient_unhook           :: Unhook
foreign export ccall ircclient_hook_command     :: HookCommand
#endif


loadCommand :: Command
loadCommand path cl =
  do dl         <- dlopen (Text.unpack path) [RTLD_NOW, RTLD_LOCAL]
     fpStartup  <- dynStartup  <$> dlsym dl "startup_entry"
     fpMessage  <- dynMessage  <$> dlsym dl "message_entry"
     fpShutdown <- dynShutdown <$> dlsym dl "shutdown_entry"

     extIdRef'  <- newIORef (error "ext id not initialized")
     userRef'   <- newIORef nullPtr
     mvar       <- newEmptyMVar
     hooksRef'  <- newIORef Bag.empty

     let cext = CExtension
           { extIdRef   = extIdRef'
           , hooksRef   = hooksRef'
           , userRef    = userRef'
           , clientMVar = mvar
           }

     stable <- newStablePtr cext

     let (i, cl1) = addExtension cl
           Extension
             { _onMessage  = cMessage cext fpMessage
             , _onShutdown = cShutdown cext stable fpShutdown dl
             , _onCommand  = HookMap.empty
             }

     writeIORef extIdRef' i

     (userPtr, cl2) <-
       parked cext cl1 \_ptr ->
         fpStartup (castStablePtrToPtr stable)

     if nullPtr == userPtr then
       do let cl3 = addLine ("Failed to initialize: " <> path) cl2
          return (Continue, cl3)
     else
       do writeIORef (userRef cext) userPtr
          let Bag.Key j = i
          let cl3 = addLine ("Loaded: #" <> Text.pack (show j) <> " " <> path) cl2
          return (Continue, cl3)

parked :: CExtension -> Client -> (Ptr () -> IO a) -> IO (a, Client)
parked extSt cl k =
  do putMVar (clientMVar extSt) cl
     res <- k =<< readIORef (userRef extSt)
     cl1 <- takeMVar (clientMVar extSt)
     return (res, cl1)

cMessage ::
  CExtension ->
  MessageFun ->
  Client ->
  Text -> IO (NextStep, Client)
cMessage cext fp cl msg =
  Text.withCStringLen msg \(msgptr, msglen) ->
  parked cext cl \ptr ->
  fmap convertResult $
  fp ptr msgptr (fromIntegral msglen)

convertResult :: CInt -> NextStep
convertResult 1 = Skip
convertResult 2 = Quit
convertResult _ = Continue

cShutdown ::
  CExtension ->
  StablePtr CExtension ->
  ShutdownFun ->
  DL ->
  Client ->
  IO Client
cShutdown cext stab fp dl cl =
  fmap snd $
  parked cext cl \ptr ->
  do fp ptr
     dlclose dl
     freeStablePtr stab


reentry :: Ptr () -> (CExtension -> Client -> IO (Client, a)) -> IO a
reentry stab k =
  do cext <- deRefStablePtr (castPtrToStablePtr stab) :: IO CExtension
     modifyMVar (clientMVar cext) \cl ->
        do (cl', result) <- k cext cl
           return (cl', result)

type Print = Ptr () -> CString -> CSize -> IO ()
ircclient_print :: Print
ircclient_print stab strPtr strLen =
  do str <- Text.peekCStringLen (strPtr, fromIntegral strLen)
     reentry stab \_i cl ->
       return (over (clUI . uiLines) (str :) cl, ())

type Send = Ptr () -> CString -> CSize -> CString -> CSize -> IO CInt
ircclient_send :: Send
ircclient_send stab netPtr netLen strPtr strLen =
  do net <- Text.peekCStringLen (netPtr, fromIntegral netLen)
     str <- Text.peekCStringLen (strPtr, fromIntegral strLen)
     reentry stab \_i cl ->
       case view (clConns . at net) cl of
         Nothing -> return (cl, 1)
         Just conn ->
           do sendConnection conn str
              return (cl, 0)

type Unhook = Ptr () -> CLong -> IO (Ptr ())
ircclient_unhook :: Unhook
ircclient_unhook token hookId =
  reentry token \cext cl ->
  do let key = Bag.Key (fromIntegral hookId)
     extId <- readIORef (extIdRef cext)
     hooks <- readIORef (hooksRef cext)
     writeIORef (hooksRef cext) (Bag.delete key hooks)
     case preview (ix key) hooks of
       Nothing -> return (cl, nullPtr)
       Just (CommandHook i ptr) ->
         return (cl & clExts . ix extId . onCommand %~ HookMap.remove i, ptr)


type CommandCb =
  Ptr CString -> Ptr CSize ->
  Ptr CString -> Ptr CSize ->
  CSize ->
  Ptr () ->
  IO CInt

type HookCommand =
  Ptr () ->
  CString -> CSize ->
  CInt -> FunPtr CommandCb ->
  CString -> CSize ->
  Ptr () ->
  IO CLong
ircclient_hook_command :: HookCommand
ircclient_hook_command token namePtr nameLen priority fp _helpPtr _helpLen userPtr =
  Text.peekCStringLen (namePtr, fromIntegral nameLen) >>= \name ->
  reentry token \cext cl ->
    do extId <- readIORef (extIdRef cext)
       let action = commandWrapper cext userPtr fp
           (i, cl1) = cl & clExts . singular (ix extId) . onCommand
                        %%~ HookMap.insert (fromIntegral priority) name action

       hooks <- readIORef (hooksRef cext)
       let (Bag.Key hookId, hooks') = Bag.insert (CommandHook i userPtr) hooks
       writeIORef (hooksRef cext) hooks'

       return (cl1, fromIntegral hookId)

commandWrapper ::
  CExtension ->
  Ptr () ->
  FunPtr CommandCb ->
  Text ->
  Client ->
  IO (NextStep, Client)
commandWrapper cext userPtr fp txt cl =
  runWithIO (exportArgs txt) \(wordsPtr, wordsLenPtr, eolPtr, eolLenPtr, args) ->
  parked cext cl   \_ptr ->
  convertResult <$>
  dynCommandCb fp wordsPtr wordsLenPtr eolPtr eolLenPtr args userPtr

exportArgs :: Text -> WithIO (Ptr CString, Ptr CSize, Ptr CString, Ptr CSize, CSize)
exportArgs txt =
  do let ws = Text.words txt
         wsEol = eolWords txt

     (ptrs   , sizes   ) <- unzip <$> traverse exportText ws
     (ptrsEol, sizesEol) <- unzip <$> traverse exportText wsEol

     ptrsArr     <- exportArray ptrs
     sizesArr    <- exportArray sizes
     ptrsEolArr  <- exportArray ptrsEol
     sizesEolArr <- exportArray sizesEol

     return (ptrsArr, sizesArr,
             ptrsEolArr, sizesEolArr,
             fromIntegral (length ws))

eolWords :: Text -> [Text]
eolWords start = go (noSpaces start)
  where
    noSpaces = Text.dropWhile (' '==)
    toSpaces = Text.dropWhile (' '/=)

    go txt
      | Text.null txt = []
      | otherwise = txt : go (noSpaces (toSpaces txt))
