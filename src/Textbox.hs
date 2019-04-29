{-# Language TemplateHaskell, BlockArguments #-}
module Textbox where

import Control.Lens
import Data.Foldable
import Data.Sequence (Seq)
import Graphics.Vty
import qualified Data.Sequence as Seq

data Textbox = Textbox
  { _tbContent :: !(Seq Char)
  , _tbCursor  :: !Int
  }

makeLenses ''Textbox

tbString :: Textbox -> String
tbString = views tbContent toList

emptyTextbox :: Textbox
emptyTextbox = Textbox
  { _tbContent = Seq.empty
  , _tbCursor  = 0
  }

tbInsert :: Char -> Textbox -> Textbox
tbInsert c = over tbContent (|> c)

tbDeleteBack :: Textbox -> Textbox
tbDeleteBack = over tbContent \s -> Seq.take (Seq.length s - 1) s

tbImage :: Int -> Textbox -> Image
tbImage w tb = string defAttr (toList s)
  where
    n = Seq.length (view tbContent tb)
    s = Seq.drop (max 0 (n - w)) (view tbContent tb)
