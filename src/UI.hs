{-# Language TemplateHaskell, BlockArguments, OverloadedStrings #-}
module UI where

import Control.Lens
import Data.Text (Text)
import Graphics.Vty

import Textbox

data UI = UI
  { _uiTextbox   :: !Textbox
  , _uiLines     :: [Text]
  , _uiHeight    :: !Int
  , _uiWidth     :: !Int
  , _uiFocus     :: !(Maybe Text)
  }

makeLenses ''UI

emptyUI ::
  Int {- ^ width  -} ->
  Int {- ^ height -} ->
  UI
emptyUI w h = UI
  { _uiTextbox   = emptyTextbox
  , _uiLines     = []
  , _uiHeight    = h
  , _uiWidth     = w
  , _uiFocus     = Nothing
  }

uiImage :: UI -> Image
uiImage ui =
  padding <->
  textlines <->
  statusline <->
  textbox

  where
    clientH = view uiHeight ui
    textH   = min (clientH - 2) (length (view uiLines ui))
    padH    = max 0 (clientH - textH - 2)

    padding = charFill defAttr ' ' 1 padH

    statusline =
      case view uiFocus ui of
        Nothing -> text' defAttr "No focus"
        Just x  -> text' defAttr ("Focus: " <> x)

    textlines
      = foldr (\l ls -> ls <-> l) emptyImage
      $ map (text' defAttr)
      $ take textH
      $ view uiLines ui

    textbox = tbImage (view uiWidth ui) (view uiTextbox ui)

uiPicture :: UI -> Picture
uiPicture ui =
  Picture
    { picCursor = NoCursor
    , picLayers = [uiImage ui]
    , picBackground = ClearBackground
    }

clearTextbox :: UI -> UI
clearTextbox = set uiTextbox emptyTextbox
