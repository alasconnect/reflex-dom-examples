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
  el "h1" $ text "Morris.js integration with Reflex DOM!"
