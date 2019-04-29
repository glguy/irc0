{-# Language OverloadedStrings #-}
module Connection where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Text (Text)
import Hookup
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

data IrcConnection = IrcConnection
  { connEvents :: TQueue NetEvent
  , connInput  :: TQueue Text
  , connThread :: Async ()
  }

data NetEvent
  = NetEnd Text
  | NetMessage Text

newConnection :: IO IrcConnection
newConnection =
  do o <- newTQueueIO
     i <- newTQueueIO
     t <- async (startConnection o i)
     _ <- forkIO (atomically . writeTQueue o . NetEnd . Text.pack . show =<< waitCatch t)
     return (IrcConnection o i t)

params :: ConnectionParams
params =
  ConnectionParams
    { cpFamily = defaultFamily
    , cpHost = "chat.freenode.net"
    , cpPort = 9003
    , cpSocks = Nothing
    , cpTls = Just defaultTlsParams
    }

cancelConnection :: IrcConnection -> IO ()
cancelConnection = cancel . connThread

sendConnection :: IrcConnection -> Text -> IO ()
sendConnection c txt = atomically (writeTQueue (connInput c) txt)

startConnection :: TQueue NetEvent -> TQueue Text -> IO ()
startConnection o i =
  do h <- connect params
     race_ (reader h o) (writer h i)

reader :: Connection -> TQueue NetEvent -> IO ()
reader h o =
  do mbBs <- recvLine h 1024
     case mbBs of
       Nothing -> return ()
       Just bs ->
         case Text.decodeUtf8' bs of
           Left e -> throwIO e
           Right t -> do atomically (writeTQueue o (NetMessage t))
                         reader h o

writer :: Connection -> TQueue Text -> IO a
writer h i =
  forever $
  do chunk <- atomically (readTQueue i)
     send h (Text.encodeUtf8 (chunk <> "\r\n"))
