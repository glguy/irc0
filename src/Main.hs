{-# Language OverloadedStrings #-}
{-# Options_GHC -Wno-unused-do-bind -Wno-name-shadowing #-}
module Main where

import Control.Exception
import Control.Lens
import Graphics.Vty

import Client
import EventLoop
import Extension.CApi (loadCommand)

withVty :: Config -> (Vty -> IO a) -> IO a
withVty config = bracket (mkVty config) shutdown

main :: IO ()
main =
  do config <- standardIOConfig
     withVty config $ \vty ->
       do (w,h) <- displayBounds (outputIface vty)
          let cl = newClient w h
                 & clCommands . at "load" ?~ loadCommand
          eventLoop vty cl
