module Layout (runLayout) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import GHCJS.DOM.Types (JSM)
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
  
  elAttr "ul" ("style" =: "list-style-type:none;min-width:600px") $ do
    elAttr "li" ("style" =: "width:50%;float:left") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "topLeft") blank
    elAttr "li" ("style" =: "width:50%;float:left;clear:right") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "topRight") blank
    elAttr "li" ("style" =: "width:50%;float:left") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "bottomLeft") blank
    elAttr "li" ("style" =: "width:50%;float:left;clear:right") $ 
      elAttr "div" ("class" =: "ct-chart ct-golden-section" <> "id" =: "bottomRight") blank

  _ <- perform (const $ makeLineChart lineData) =<< delay 0 =<< getPostBuild
  _ <- perform (const $ makeBarChart barData) =<< delay 0 =<< getPostBuild
  _ <- perform (const $ makePieChart pieData) =<< delay 0 =<< getPostBuild
  _ <- perform (const $ makeDonutChart donutData) =<< delay 0 =<< getPostBuild

  --e <- getPostBuild
  --e' <- delay 0 e
  --_ <- performEvent $ liftIO makeLineChart <$ e'

  return ()

-- Invoke JS FFI (developed by Chris, based on Reflex source code)
perform :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
perform f x = performEvent (fmap (liftIO . f) x)

-- Data to display in charts
lineData :: MultiDataSet
lineData 
  = MultiDataSet 
  { mLabels = ["Week1","Week2","Week3","Week4","Week5","Week6","Week7","Week8","Week9"]
  , mSeries = [[12,9,7,8,6,4,3,2,0],[2,1,3.5,7,9,8,7.7,4,7],[1,3,4,5,6,8,9,10,11],[11,7.5,5.5,5.5,4,3.5,2,1,0]]
  }

barData :: MultiDataSet
barData 
  = MultiDataSet 
  { mLabels = ["1938","1939","1940","1941","1942","1943"]
  , mSeries = [[12000,9000,7000,8000,12000,10000],[2000,1000,3500,7000,5000,9000]]
  }

pieData :: SingleDataSet
pieData 
  = SingleDataSet 
  { sLabels = ["Day one","Day two","Day three","Day four"]
  , sSeries = [20,15,40,10]
  }

donutData :: SingleDataSet
donutData 
  = SingleDataSet 
  { sLabels = []
  , sSeries = [60,20]
  }