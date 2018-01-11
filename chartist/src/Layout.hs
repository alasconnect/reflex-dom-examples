module Layout where

import GHCJS.DOM.Types ( JSM )
import Reflex.Dom
  ( MonadWidget
  , el
  , mainWidget
  , text
  )

runLayout :: JSM ()
runLayout = mainWidget body

body :: MonadWidget t m => m ()
body = do
  el "h1" $ text "Welcome to Reflex DOM"
  el "p" $ text "Chartist is the BOMB."
