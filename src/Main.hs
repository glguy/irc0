{-# Options_GHC -Wno-unused-do-bind -Wno-name-shadowing #-}
module Main where

import Control.Exception
import Control.Lens
import Control.Monad
import Graphics.Vty

import Client
import EventLoop

withVty :: Config -> (Vty -> IO a) -> IO a
withVty config = bracket (mkVty config) shutdown

main :: IO ()
main =
  do config <- standardIOConfig
     withVty config $ \vty ->
       do (w,h) <- displayBounds (outputIface vty)
          cl    <- newClient w h
          cl    <- eventLoop vty cl
          -- quit all open connections
          foldM shutdownExtension cl (view clExts cl)
     return ()
