{-# Language BlockArguments #-}
module Extension
  ( Extension (..)
  ) where

import Data.Text (Text)

import HookMap (HookMap)
import qualified HookMap


{-
type CommandHook = [Text] -> [Text] -> IO Bool

removeCommandHook :: Int -> Extension -> (Ptr (), Extension)
removeCommandHook i ext =
  case HookMap.remove i (extCommands ext) of
    (mb, commands') -> (maybe nullPtr fst mb, ext { extCommands = commands' })
-}
