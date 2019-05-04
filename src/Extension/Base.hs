{-# Language OverloadedStrings, BlockArguments #-}
module Extension.Base where

import Control.Lens
import Data.Foldable
import qualified Irc.RawIrcMsg as Irc
import qualified Irc.Commands as Irc

import Client
import Connection
import UI
import qualified HookMap
import qualified Data.Text as Text
import Text.Read (readMaybe)
import qualified Bag as Bag

baseExtension :: Extension
baseExtension =
  let hook k v = snd . HookMap.insert 0 k v in
  newExtension

  & onCommand %~
    hook "conn"    connCommand .
    hook "exit"    exitCommand .
    hook "focus"   focusCommand .
    hook "unload"  unloadCommand

  & onMessage %~
    hook "PING"    pingMessage

pingMessage :: OnMessage
pingMessage net msg cl =
  do forOf_ (clConns . ix net) cl \conn ->
       let rsp = Irc.ircPong (view Irc.msgParams msg)
           txt = Irc.asUtf8 (Irc.renderRawIrcMsg rsp)
       in sendConnection conn txt
     return (Continue, cl)

exitCommand :: OnCommand
exitCommand _ cl = return (Quit, cl)

focusCommand :: OnCommand
focusCommand name cl =
  return (Continue, cl & clUI . uiFocus ?~ name)

connCommand :: OnCommand
connCommand key cl =
  do conn <- newConnection
     case cl & clConns . at key <<.~ Just conn of
       (old, cl1) ->
         do for_ old \oldC -> cancelConnection oldC
            return (Continue, cl1 & clUI . uiFocus .~ Just key)

unloadCommand :: OnCommand
unloadCommand arg cl =
  case readMaybe (Text.unpack arg) of
    Nothing -> return (Continue, cl)
    Just n  ->
      do cl' <- removeExtension (Bag.Key n) cl
         return (Continue, cl')
