{-# Language CPP, BlockArguments #-}
module Extension.CApi
  ( reentry
  , loadCommand

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
import NestedIO

type Wrapper a = FunPtr a -> a
type StartupFun = Ptr () -> IO (Ptr ())
type MessageFun = Ptr () -> CString -> CSize -> IO ()
type ShutdownFun = Ptr () -> IO ()

data HookEntry
  = CommandHook !Int (Ptr ())

data CExtension = CExtension
  { extIdRef   :: IORef Bag.Key
  , hooksRef   :: IORef (Bag HookEntry)
  , userRef    :: IORef (Ptr ())
  , clientMVar :: !(MVar Client)
  }

foreign import ccall "dynamic" dynStartup  :: Wrapper StartupFun
foreign import ccall "dynamic" dynMessage  :: Wrapper MessageFun
foreign import ccall "dynamic" dynShutdown :: Wrapper ShutdownFun
foreign import ccall "dynamic" dynCommandCb :: Wrapper CommandCb

#ifndef GHCI
foreign export ccall ircclient_print            :: Print
foreign export ccall ircclient_send             :: Send
foreign export ccall ircclient_unhook           :: Unhook
foreign export ccall ircclient_hook_command     :: HookCommand
#endif

{-
type CommandHook = [Text] -> [Text] -> IO Bool

removeCommandHook :: Int -> Extension -> (Ptr (), Extension)
removeCommandHook i ext =
  case HookMap.remove i (extCommands ext) of
    (mb, commands') -> (maybe nullPtr fst mb, ext { extCommands = commands' })

-}

loadCommand :: Command
loadCommand path cl =
  do dl         <- dlopen (Text.unpack path) [RTLD_NOW, RTLD_LOCAL]
     fpStartup  <- dlsym dl "startup_entry"
     fpMessage  <- dlsym dl "message_entry"
     fpShutdown <- dlsym dl "shutdown_entry"

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
             { _onMessage  = cMessage  cext fpMessage
             , _onShutdown = cShutdown cext stable fpShutdown dl
             , _onCommand  = HookMap.empty
             }

     writeIORef extIdRef' i

     cl2 <- cStartup cext stable fpStartup cl1
     return (Continue, cl2)

parked :: CExtension -> Client -> (Ptr () -> IO ()) -> IO Client
parked extSt cl k =
  do putMVar (clientMVar extSt) cl
     k =<< readIORef (userRef extSt)
     takeMVar (clientMVar extSt)

cStartup ::
  CExtension ->
  StablePtr CExtension ->
  FunPtr StartupFun ->
  Client ->
  IO Client
cStartup cext stable fp cl =
  parked cext cl \_ptr ->
    do ptr <- dynStartup fp (castStablePtrToPtr stable)
       writeIORef (userRef cext) ptr

cMessage ::
  CExtension ->
  FunPtr MessageFun ->
  Client ->
  Text -> IO Client
cMessage cext fp cl msg =
  Text.withCStringLen msg \(msgptr, msglen) ->
  parked cext cl \ptr ->
  dynMessage fp ptr msgptr (fromIntegral msglen)

cShutdown ::
  CExtension ->
  StablePtr CExtension ->
  FunPtr ShutdownFun ->
  DL ->
  Client ->
  IO Client
cShutdown cext stab fp dl cl =
  parked cext cl \ptr ->
  do dynShutdown fp ptr
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
  IO ()

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
       let (i, cl1) = cl & clExts . singular (ix extId) . onCommand
                        %%~ HookMap.insert (fromIntegral priority) name (impl cext)

       hooks <- readIORef (hooksRef cext)
       let (Bag.Key hookId, hooks') = Bag.insert (CommandHook i userPtr) hooks
       writeIORef (hooksRef cext) hooks'

       return (cl1, fromIntegral hookId)
  where
    impl cext txt cl =
        runNestedIO serial \(wordsPtr, wordsLenPtr, args) ->
      do cl' <- parked cext cl (\_ -> dynCommandCb fp wordsPtr wordsLenPtr nullPtr nullPtr args userPtr)
         return (Continue, cl')
      where
        serial :: NestedIO (Ptr CString, Ptr CSize, CSize)
        serial =
          do let ws = Text.words txt
             ptrs <- traverse (\w -> nest1 (Text.withCStringLen w)) ws
             let (a,b) = unzip ptrs
             wordsPtr <- nest1 (withArray a)
             wordsLenPtr <- nest1 (withArray (fromIntegral <$> b))
             return (wordsPtr, wordsLenPtr, fromIntegral (length ws))

 -- stab name priority callback help userdata = return 0
