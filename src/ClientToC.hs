{-# Language BlockArguments #-}
module ClientToC where

import Foreign
import Foreign.C
import System.Posix.DynamicLinker
import Data.Text (Text)
import qualified Data.Text.Foreign as Text

type StartupFun = Ptr () -> IO (Ptr ())
type CommandFun = Ptr () -> CString -> CSize -> IO ()
type MessageFun = Ptr () -> CString -> CSize -> IO ()
type ShutdownFun = Ptr () -> IO ()

data Extension a = Extension
  { extDL :: DL
  , extData     :: a
  , extStartup  :: StartupFun
  , extCommand  :: CommandFun
  , extMessage  :: MessageFun
  , extShutdown :: ShutdownFun
  }

openExtension :: FilePath -> IO (Extension ())
openExtension path =
  do dl       <- dlopen path [RTLD_NOW, RTLD_LOCAL]
     startup  <- callStartup  <$> dlsym dl "startup_entry"
     command  <- callCommand  <$> dlsym dl "command_entry"
     message  <- callCommand  <$> dlsym dl "message_entry"
     shutdown <- callShutdown <$> dlsym dl "shutdown_entry"

     return Extension
       { extDL       = dl
       , extData     = ()
       , extStartup  = startup
       , extCommand  = command
       , extMessage  = message
       , extShutdown = shutdown
       }

messageExtension :: Extension (Ptr ()) -> Text -> IO ()
messageExtension ext msg =
  Text.withCStringLen msg \(ptr, len) ->
  extMessage ext (extData ext) ptr (fromIntegral len)

type Wrapper a = FunPtr a -> a

foreign import ccall "dynamic" callStartup  :: Wrapper StartupFun
foreign import ccall "dynamic" callCommand  :: Wrapper CommandFun
foreign import ccall "dynamic" callMessage  :: Wrapper MessageFun
foreign import ccall "dynamic" callShutdown :: Wrapper ShutdownFun
