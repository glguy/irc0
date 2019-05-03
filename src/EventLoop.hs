{-# Language OverloadedStrings, TemplateHaskell, BlockArguments, EmptyCase #-}
{-# Options_GHC -Wall #-}
module EventLoop where

import Control.Lens
import Data.Foldable
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Irc.RawIrcMsg as Irc

import qualified Data.Text as Text
import qualified Data.Map as Map

import Graphics.Vty

import Textbox
import UI
import Client
import Connection
import qualified HookMap

eventLoop :: Vty -> Client -> IO ()
eventLoop vty cl =

  do update vty (uiPicture (view clUI cl))

     let vtycase = handleVtyEvent vty cl <$> readTChan (_eventChannel (inputIface vty))
         netcase name conn = handleNetEvent cl name <$> readTQueue (connEvents conn)

     action <- atomically
             $ asum
             $ vtycase
             : [ netcase name conn | (name, conn) <- Map.toList (view clConns cl) ]

     (next, cl') <- action
     case next of
       Quit     -> shutdownClient cl'
       Continue -> eventLoop vty cl'
       Skip     -> eventLoop vty cl'

handleNetEvent :: Client -> Text -> NetEvent -> IO (NextStep, Client)
handleNetEvent cl key ev =
  case ev of
    NetEnd     msg -> return (Continue, cl & clUI . uiLines %~ (msg:)
                                           & clConns . at key .~ Nothing)
    NetMessage txt ->
      case Irc.parseRawIrcMsg txt of
        Nothing -> return (Continue, cl)
        Just msg -> clientMessage key msg cl

handleVtyEvent :: Vty -> Client -> Event -> IO (NextStep, Client)
handleVtyEvent vty cl ev =
     case ev of
       EvKey (KChar c) [] -> return (Continue, over (clUI . uiTextbox) (tbInsert c) cl)
       EvKey KBS       [] -> return (Continue, over (clUI . uiTextbox) tbDeleteBack cl)
       EvKey KEnter    [] -> execute cl
       EvResize{} ->
         do (w,h) <- displayBounds (outputIface vty)
            return (Continue, cl & clUI . uiWidth  .~ w
                                 & clUI . uiHeight .~ h)
       _ -> return (Continue, cl)

execute :: Client -> IO (NextStep, Client)
execute cl =
  let cl1 = set (clUI . uiTextbox) emptyTextbox cl
      input = views (clUI . uiTextbox) tbString cl
  in
  case parseCommand input of
    Just (cmd, args) ->
      let key = Text.pack cmd
          args' = Text.pack args in
      case HookMap.lookup key (view onCommand <$> views clExts toList cl) of
        [] -> return (Continue, cl) -- leaves command in place
        hs -> runHandlers (map ($ args') hs) cl1
    Nothing ->
      case view (clUI . uiFocus) cl of
        Nothing -> return (Continue, cl)
        Just key ->
          case view (clConns . at key) cl of
            Nothing -> return (Continue, cl)
            Just conn ->
               do sendConnection conn (Text.pack input)
                  return (Continue, cl1)


parseCommand :: String -> Maybe (String, String)
parseCommand str
  | '/':str1   <- dropWhile (' '==) str
  , (cmd,args) <- break (' '==) str1
  , let args1 = drop 1 args = Just (cmd, args1)

  | otherwise = Nothing
