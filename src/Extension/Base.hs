{-# Language OverloadedStrings, BlockArguments #-}
module Extension.Base where

import Control.Lens
import Data.Foldable

import Client
import Connection
import UI
import qualified HookMap
import qualified Data.Text as Text
import Text.Read (readMaybe)
import qualified Bag as Bag

baseExtension :: Extension
baseExtension =
  let cmd k v = snd . HookMap.insert 0 k v in
  newExtension & onCommand %~
    cmd "conn"    connCommand .
    cmd "exit"    exitCommand .
    cmd "focus"   focusCommand .
    cmd "unload"  unloadCommand

exitCommand :: Command
exitCommand _ cl = return (Quit, cl)

focusCommand :: Command
focusCommand name cl =
  return (Continue, cl & clUI . uiFocus ?~ name)

connCommand :: Command
connCommand key cl =
  do conn <- newConnection
     case cl & clConns . at key <<.~ Just conn of
       (old, cl1) ->
         do for_ old \oldC -> cancelConnection oldC
            return (Continue, cl1 & clUI . uiFocus .~ Just key)

unloadCommand :: Command
unloadCommand arg cl =
  case readMaybe (Text.unpack arg) of
    Nothing -> return (Continue, cl)
    Just n  ->
      do cl' <- removeExtension (Bag.Key n) cl
         return (Continue, cl')
