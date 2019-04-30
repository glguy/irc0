{-# Language BlockArguments #-}
module Extension
  ( Extension
  , Extension'

  , open
  , onMessage
  , onCommand
  , startup
  , shutdown
  ) where

import Foreign
import Foreign.C
import System.Posix.DynamicLinker
import Data.Text (Text)
import qualified Data.Text.Foreign as Text

type Extension = Extension' (Ptr ())

type StartupFun = Ptr () -> IO (Ptr ())
type CommandFun = Ptr () -> CString -> CSize -> IO ()
type MessageFun = Ptr () -> CString -> CSize -> IO ()
type ShutdownFun = Ptr () -> IO ()

data Extension' a = Extension
  { extDL       :: DL
  , extData     :: a
  , extStartup  :: StartupFun
  , extCommand  :: CommandFun
  , extMessage  :: MessageFun
  , extShutdown :: ShutdownFun
  }

open :: FilePath -> IO (Extension' ())
open path = -- XXX: error handling
  do dl         <- dlopen path [RTLD_NOW, RTLD_LOCAL]
     fpStartup  <- dlsym dl "startup_entry"
     fpCommand  <- dlsym dl "command_entry"
     fpMessage  <- dlsym dl "message_entry"
     fpShutdown <- dlsym dl "shutdown_entry"

     return Extension
       { extDL       = dl
       , extData     = ()
       , extStartup  = dynStartup  fpStartup
       , extCommand  = dynCommand  fpCommand
       , extMessage  = dynMessage  fpMessage
       , extShutdown = dynShutdown fpShutdown
       }

onMessage :: Extension -> Text -> IO ()
onMessage ext msg =
  Text.withCStringLen msg \(ptr, len) ->
  extMessage ext (extData ext) ptr (fromIntegral len)

onCommand :: Extension -> Text -> IO ()
onCommand ext msg =
  Text.withCStringLen msg \(ptr, len) ->
  extCommand ext (extData ext) ptr (fromIntegral len)

startup :: Ptr () -> Extension' () -> IO Extension
startup clPtr ext =
  do ptr <- extStartup ext clPtr
     return ext {extData = ptr}

shutdown :: Extension -> IO ()
shutdown ext =
  do extShutdown ext (extData ext)
     dlclose (extDL ext)


type Wrapper a = FunPtr a -> a

foreign import ccall "dynamic" dynStartup  :: Wrapper StartupFun
foreign import ccall "dynamic" dynCommand  :: Wrapper CommandFun
foreign import ccall "dynamic" dynMessage  :: Wrapper MessageFun
foreign import ccall "dynamic" dynShutdown :: Wrapper ShutdownFun
