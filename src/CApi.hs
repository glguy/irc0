{-# Language CPP, BlockArguments #-}
#ifdef GHCI
{-# Options -Wno-unused-top-binds #-}
#endif
module CApi () where

import Control.Lens
import Foreign
import Foreign.C
import qualified Data.Text.Foreign as Text

import Client
import UI
import Connection


#ifndef GHCI
foreign export ccall ircclient_print :: Print
foreign export ccall ircclient_send  :: Send
#endif

type Print = Ptr () -> CString -> CSize -> IO ()
ircclient_print :: Print
ircclient_print stab strPtr strLen =
  do str <- Text.peekCStringLen (strPtr, fromIntegral strLen)
     reentry stab \cl ->
       return (over (clUI . uiLines) (str :) cl, ())

type Send = Ptr () -> CString -> CSize -> CString -> CSize -> IO CInt
ircclient_send :: Send
ircclient_send stab netPtr netLen strPtr strLen =
  do net <- Text.peekCStringLen (netPtr, fromIntegral netLen)
     str <- Text.peekCStringLen (strPtr, fromIntegral strLen)
     reentry stab \cl ->
       case view (clConns . at net) cl of
         Nothing -> return (cl, 1)
         Just conn ->
           do sendConnection conn str
              return (cl, 0)
