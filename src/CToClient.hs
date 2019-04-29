{-# Language CPP, BlockArguments #-}
module CToClient where

import Control.Lens
import Foreign
import Foreign.C
import qualified Data.Text.Foreign as Text

import Client
import UI

type AddMessage = Ptr () -> CString -> CSize -> IO ()

#ifndef GHCI
foreign export ccall "ircclient_add_message" addMessage :: AddMessage
#endif

addMessage :: AddMessage
addMessage stab strPtr strLen =
  reentry stab \cl ->
    do str <- Text.peekCStringLen (strPtr, fromIntegral strLen)
       let cl' = over (clUI . uiLines) (str :) cl
       return cl'
