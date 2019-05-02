{-# Language OverloadedStrings, BlockArguments #-}
module Extension.Base where

import Control.Lens
import Data.Foldable

import Client
import Connection
import UI
import qualified HookMap

baseExtension :: Extension
baseExtension =
  newExtension & onCommand %~
    snd . HookMap.insert 0 "conn" connCommand .
    snd . HookMap.insert 0 "exit" exitCommand .
    snd . HookMap.insert 0 "focus" focusCommand

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
