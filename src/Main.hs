module Main where

import Control.Exception
import Control.Lens
import Control.Monad
import Graphics.Vty

import Client
import EventLoop
import UI

withVty :: Config -> (Vty -> IO a) -> IO a
withVty config = bracket (mkVty config) shutdown

main :: IO ()
main =
  do config <- standardIOConfig
     cl <-
       withVty config $ \vty ->
         do (w,h) <- displayBounds (outputIface vty)
            let ui = emptyUI w h
            cl <- newClient ui
            eventLoop vty cl
     _cl' <- foldM shutdownExtension cl (view clExts cl)
     return ()
