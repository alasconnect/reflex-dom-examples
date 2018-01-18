module Layout where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Monoid ( (<>) )
import GHCJS.DOM.Types ( JSM )
import JavascriptTools
import Reflex 
  ( Event
  , delay
  , getPostBuild
  , performEvent )
import Reflex.Dom
  ( (=:)
  , MonadWidget
  , blank
  , el
  , elAttr
  , mainWidget
  , text )

runLayout :: JSM ()
runLayout = mainWidget body

-- Build the body of the page
body :: (MonadWidget t m, MonadIO m) => m ()
body = do
  el "h1" $ text "Chartist.js integration with Reflex DOM!"
  
  elAttr "ul" ("style" =: "list-style-type:none") $ do
    elAttr "li" ("style" =: "width:50%;float:left") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "topLeft") blank
    elAttr "li" ("style" =: "width:50%;float:left;clear:right") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "topRight") blank
    elAttr "li" ("style" =: "width:50%;float:left") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "bottomLeft") blank
    elAttr "li" ("style" =: "width:50%;float:left;clear:right") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "bottomRight") blank

  _ <- perform (const $ makeLineChart lData) =<< delay 0 =<< getPostBuild
  --e <- getPostBuild
  --e' <- delay 0 e
  --_ <- performEvent $ liftIO makeLineChart <$ e'

  return ()

-- Invoke JS FFI (developed by Chris, based on Reflex source code)
perform :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
perform f x = performEvent (fmap (liftIO . f) x)

-- Data to display in charts
lData :: ChartData
lData = ChartData {labels = ["Mon","Tue","Wed","Thu","Fri","Sat"], series = [[5,4,3,7,5,10],[3,2,9,5,4,6],[2,1,-3,-4,-2,0]]}