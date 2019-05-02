{-# Language OverloadedStrings #-}
{-# Options_GHC -Wno-unused-do-bind -Wno-name-shadowing #-}
module Main where

import Control.Exception
import Graphics.Vty

import Client
import EventLoop
import Extension.Base (baseExtension)
import Extension.CApi (capiExtension)
import qualified Bag
import Control.Lens

withVty :: Config -> (Vty -> IO a) -> IO a
withVty config = bracket (mkVty config) shutdown

main :: IO ()
main =
  do config <- standardIOConfig
     withVty config $ \vty ->
       do (w,h) <- displayBounds (outputIface vty)
          let cl = newClient w h
                 & clExts %~ snd . Bag.insert baseExtension
                           . snd . Bag.insert capiExtension
          eventLoop vty cl
