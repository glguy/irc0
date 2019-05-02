{-# Language OverloadedStrings, TemplateHaskell, BlockArguments, EmptyCase #-}
{-# Options_GHC -Wall #-}
module EventLoop where

import Control.Lens
import Data.Foldable
import Control.Concurrent.STM
import Data.Text (Text)

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

     let vtycase = handleVtyEvent cl <$> readTChan (_eventChannel (inputIface vty))
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
    NetMessage msg ->
      do cl' <- clientMessage msg cl
         return (Continue, cl')

handleVtyEvent :: Client -> Event -> IO (NextStep, Client)
handleVtyEvent cl ev =
     case ev of
       EvKey (KChar c) [] -> return (Continue, over (clUI . uiTextbox) (tbInsert c) cl)
       EvKey KBS       [] -> return (Continue, over (clUI . uiTextbox) tbDeleteBack cl)
       EvKey KEnter    [] -> execute cl
       EvResize w h       -> return (Continue, cl & clUI . uiWidth  .~ w
                                                  & clUI . uiHeight .~ h)
       _                  -> return (Continue, cl)

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

runHandlers :: [Client -> IO (NextStep, Client)] -> Client -> IO (NextStep, Client)
runHandlers [] cl = return (Continue, cl)
runHandlers (h:hs) cl =
  do (next, cl') <- h cl
     case next of
       Continue -> runHandlers hs cl'
       Quit     -> return (Quit, cl')
       Skip     -> return (Skip, cl')

parseCommand :: String -> Maybe (String, String)
parseCommand str
  | '/':str1   <- dropWhile (' '==) str
  , (cmd,args) <- break (' '==) str1
  , let args1 = drop 1 args = Just (cmd, args1)

  | otherwise = Nothing
