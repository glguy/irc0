{-# Language OverloadedStrings, CPP, BlockArguments #-}
module Extension.CApi
  ( reentry
  , capiExtension

#ifdef GHCI
  , ircclient_print
  , ircclient_send
  , ircclient_unhook
  , ircclient_hook_command
  , ircclient_hook_message
  , ircclient_query
#endif
  ) where

import Foreign
import Foreign.C
import System.Posix.DynamicLinker (DL, dlopen, dlclose, dlsym, RTLDFlags(..))
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar, modifyMVar)
import Control.Exception (bracketOnError)
import Data.Char (isSpace)
import Data.IORef
import Data.Text (Text)
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Text.Foreign as Text
import qualified Data.Text as Text
import qualified Data.Map as Map

import Client
import Connection (sendConnection)
import UI (uiLines)
import qualified HookMap
import qualified Bag
import Bag (Bag)
import WithIO
import qualified Irc.RawIrcMsg as Irc
import qualified Irc.UserInfo as Irc

capiExtension :: Extension
capiExtension = newExtension & onCommand %~
  snd . HookMap.insert 0 "load" loadCommand

type Wrapper a = FunPtr a -> a
type StartupFun = Ptr () -> IO (Ptr ())

type ShutdownFun = Ptr () -> IO ()

data HookEntry
  = CommandHook !Text !Int !Bag.Key (Ptr ())
  | MessageHook !Text !Int !Bag.Key (Ptr ())

data CExtension = CExtension
  { extIdRef   :: {-# Unpack #-} !(IORef Bag.Key)
  , hooksRef   :: {-# Unpack #-} !(IORef (Bag HookEntry))
  , userRef    :: {-# Unpack #-} !(IORef (Ptr ()))
  , clientMVar :: {-# Unpack #-} !(MVar Client)
  }

foreign import ccall "dynamic" dynStartup   :: Wrapper StartupFun
foreign import ccall "dynamic" dynShutdown  :: Wrapper ShutdownFun
foreign import ccall "dynamic" dynCommandCb :: Wrapper CommandCb
foreign import ccall "dynamic" dynMessageCb :: Wrapper MessageCb
foreign import ccall "dynamic" dynStringsCb :: Wrapper StringsCb

#ifndef GHCI
foreign export ccall ircclient_print            :: Print
foreign export ccall ircclient_send             :: Send
foreign export ccall ircclient_unhook           :: Unhook
foreign export ccall ircclient_hook_command     :: HookCommand
foreign export ccall ircclient_hook_message     :: HookMessage
foreign export ccall ircclient_query            :: Query
#endif


loadCommand :: OnCommand
loadCommand path cl =
  bracketOnError
    (dlopen (Text.unpack path) [RTLD_NOW, RTLD_LOCAL])
    dlclose
    \dl ->
  do fpStartup  <- dynStartup  <$> dlsym dl "startup_entry"
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
             { _onShutdown = cShutdown cext stable fpShutdown dl
             , _onMessage  = HookMap.empty
             , _onCommand  = HookMap.empty
             }

     writeIORef extIdRef' i

     (userPtr, cl2) <-
       parked cext cl1 (fpStartup (castStablePtrToPtr stable))

     if nullPtr == userPtr then
       do let cl3 = addLine ("Failed to initialize: " <> path) cl2
          return (Continue, cl3)
     else
       do writeIORef (userRef cext) userPtr
          let Bag.Key j = i
          let cl3 = addLine ("Loaded: #" <> Text.pack (show j) <> " " <> path) cl2
          return (Continue, cl3)

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
  parked cext cl $
  do fp =<< readIORef (userRef cext)
     dlclose dl
     freeStablePtr stab

------------------------------------------------------------------------

reentry :: Ptr () -> (CExtension -> Client -> IO (a, Client)) -> IO a
reentry stab k =
  do cext <- deRefStablePtr (castPtrToStablePtr stab) :: IO CExtension
     modifyMVar (clientMVar cext) \cl ->
        do (cl', result) <- k cext cl
           return (result, cl')

parked :: CExtension -> Client -> IO a -> IO (a, Client)
parked extSt cl k =
  do putMVar (clientMVar extSt) cl
     res <- k
     cl1 <- takeMVar (clientMVar extSt)
     return (res, cl1)

------------------------------------------------------------------------

type Print = Ptr () -> CString -> CSize -> IO ()
ircclient_print :: Print
ircclient_print stab strPtr strLen =
  do str <- peekText strPtr strLen
     reentry stab \_i cl ->
       return ((), over (clUI . uiLines) (str :) cl)

------------------------------------------------------------------------

type Send = Ptr () -> CString -> CSize -> CString -> CSize -> IO CInt
ircclient_send :: Send
ircclient_send stab netPtr netLen strPtr strLen =
  do net <- peekText netPtr netLen
     str <- peekText strPtr strLen
     reentry stab \_i cl ->
       case view (clConns . at net) cl of
         Nothing -> return (1, cl)
         Just conn ->
           do sendConnection conn str
              return (0, cl)

------------------------------------------------------------------------

type Unhook = Ptr () -> CLong -> IO (Ptr ())
ircclient_unhook :: Unhook
ircclient_unhook token hookId =
  reentry token \cext cl ->
  do let key = Bag.Key (fromIntegral hookId)
     extId <- readIORef (extIdRef cext)
     hooks <- readIORef (hooksRef cext)
     writeIORef (hooksRef cext) (Bag.delete key hooks)
     case preview (ix key) hooks of
       Nothing -> return (nullPtr, cl)
       Just (CommandHook cmd prio i ptr) ->
         return (ptr, cl & clExts . ix extId . onCommand %~ HookMap.remove cmd prio i)
       Just (MessageHook cmd prio i ptr) ->
         return (ptr, cl & clExts . ix extId . onMessage %~ HookMap.remove cmd prio i)

------------------------------------------------------------------------

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
ircclient_hook_command token namePtr nameLen prio fp _helpPtr _helpLen userPtr =
  peekText namePtr nameLen >>= \name ->
  let priority = fromIntegral prio in
  reentry token \cext cl ->
    do extId <- readIORef (extIdRef cext)
       let action = commandWrapper cext userPtr (dynCommandCb fp)
           (i, cl1) = cl & clExts . singular (ix extId) . onCommand
                        %%~ HookMap.insert priority name action

       hooks <- readIORef (hooksRef cext)
       let (Bag.Key hookId, hooks') = Bag.insert (CommandHook name priority i userPtr) hooks
       writeIORef (hooksRef cext) hooks'

       return (fromIntegral hookId, cl1)

commandWrapper ::
  CExtension ->
  Ptr () ->
  CommandCb ->
  Text ->
  Client ->
  IO (NextStep, Client)
commandWrapper cext userPtr fp txt cl =
  evalWithIO $
  do let ws = Text.words txt
     (wordsPtr, wordsLenPtr, args) <- exportTexts ws
     (eolPtr, eolLenPtr, _) <- exportTexts (eolWords txt)
     liftIO $
       parked cext cl $
       convertResult <$>
       fp wordsPtr wordsLenPtr eolPtr eolLenPtr args userPtr


------------------------------------------------------------------------

type MessageCb =
  CString -> CSize ->
  CString -> CSize ->
  CString -> CSize ->
  Ptr CString -> Ptr CSize ->
  CSize ->
  Ptr () ->
  IO CInt


type HookMessage =
  Ptr () ->
  CString -> CSize ->
  CInt -> FunPtr MessageCb ->
  Ptr () ->
  IO CLong
ircclient_hook_message :: HookMessage
ircclient_hook_message token namePtr nameLen prio fp userPtr =
  peekText namePtr nameLen >>= \name ->
  let priority = fromIntegral prio in
  reentry token \cext cl ->
    do extId <- readIORef (extIdRef cext)
       let action = cMessage cext userPtr (dynMessageCb fp)
           (i, cl1) = cl & clExts . singular (ix extId) . onMessage
                        %%~ HookMap.insert priority name action

       hooks <- readIORef (hooksRef cext)
       let (Bag.Key hookId, hooks') = Bag.insert (MessageHook name priority i userPtr) hooks
       writeIORef (hooksRef cext) hooks'

       return (fromIntegral hookId, cl1)

cMessage ::
  CExtension ->
  Ptr () ->
  MessageCb ->
  Text ->
  Irc.RawIrcMsg ->
  Client ->
  IO (NextStep, Client)
cMessage cext ptr fp net msg cl =
  evalWithIO $
    do (netPtr,netLen) <- exportText net
       (pfxPtr,pfxLen) <- exportText (maybe "" Irc.renderUserInfo (view Irc.msgPrefix msg))
       (cmdPtr,cmdLen) <- exportText (view Irc.msgCommand msg)
       (argPtrsArr, argLensArr, n) <- exportTexts (view Irc.msgParams msg)
       liftIO $
         parked cext cl $
         fmap convertResult $
         fp netPtr netLen
            pfxPtr pfxLen
            cmdPtr cmdLen
            argPtrsArr argLensArr
            n
            ptr


------------------------------------------------------------------------

type StringsCb = Ptr CString -> Ptr CSize -> CSize -> Ptr () -> IO ()

type Query = Ptr () -> CString -> CSize -> FunPtr StringsCb -> Ptr () -> IO CInt
ircclient_query :: Query
ircclient_query stab keyPtr keyLen cb user =
  reentry stab \cext cl ->
  do key <- peekText keyPtr keyLen

     mbTxts <-
       case key of
         "connections" -> return (Just (Map.keys (view clConns cl)))
         _             -> return Nothing

     case mbTxts of
       Nothing -> return (1, cl)
       Just txts ->
         evalWithIO $
         exportTexts txts >>= \(strs, sizes, len) ->
         liftIO (parked cext cl (0 <$ dynStringsCb cb strs sizes len user))


------------------------------------------------------------------------

peekText :: CString -> CSize -> IO Text
peekText ptr size = Text.peekCStringLen (ptr, fromIntegral size)

exportTexts :: [Text] -> WithIO (Ptr CString, Ptr CSize, CSize)
exportTexts txts =
  do (ptrs, sizes) <- unzip <$> traverse exportText txts
     ptrArr        <- exportArray ptrs
     sizArr        <- exportArray sizes
     return (ptrArr, sizArr, fromIntegral (length txts))

eolWords :: Text -> [Text]
eolWords start = go (noSpaces start)
  where
    noSpaces = Text.dropWhile isSpace
    toSpaces = Text.dropWhile (not . isSpace)

    go txt
      | Text.null txt = []
      | otherwise = txt : go (noSpaces (toSpaces txt))
