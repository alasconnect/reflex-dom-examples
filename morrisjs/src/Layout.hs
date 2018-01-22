module Layout (runLayout) where

import Control.Monad.IO.Class (MonadIO(..))
--import Data.Monoid ((<>))
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
  el "h1" $ text "Morris.js integration with Reflex DOM!"

  elAttr "ul" ("style" =: "list-style-type:none;min-width:600px") $ do
    elAttr "li" ("style" =: "width:50%;float:left") $
      elAttr "div" ("id" =: "topLeft") blank
    elAttr "li" ("style" =: "width:50%;float:left;clear:right") $
      elAttr "div" ("id" =: "topRight") blank
    elAttr "li" ("style" =: "width:50%;float:left") $
      elAttr "div" ("id" =: "bottomLeft") blank
    elAttr "li" ("style" =: "width:50%;float:left;clear:right") $
      elAttr "div" ("id" =: "bottomRight") blank

  _ <- perform (const $ makeLineChart lineData) =<< delay 0 =<< getPostBuild
  --_ <- perform (const $ makeBarChart barData) =<< delay 0 =<< getPostBuild
  --_ <- perform (const $ makePieChart pieData) =<< delay 0 =<< getPostBuild
  --_ <- perform (const $ makeDonutChart donutData) =<< delay 0 =<< getPostBuild

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
  { mData = [ Four MDataPoint4 { bX = "Week1", bY1 = 12, bY2 = 2, bY3 = 1, bY4 = 11},
              Four MDataPoint4 { bX = "Week2", bY1 = 9, bY2 = 1, bY3 = 3, bY4 = 7.5},
              Four MDataPoint4 { bX = "Week3", bY1 = 7, bY2 = 3.5, bY3 = 4, bY4 = 5.5},
              Four MDataPoint4 { bX = "Week4", bY1 = 8, bY2 = 7, bY3 = 5, bY4 = 5.5},
              Four MDataPoint4 { bX = "Week5", bY1 = 6, bY2 = 9, bY3 = 6, bY4 = 4},
              Four MDataPoint4 { bX = "Week6", bY1 = 4, bY2 = 8, bY3 = 8, bY4 = 3.5},
              Four MDataPoint4 { bX = "Week7", bY1 = 3, bY2 = 7.7, bY3 = 9, bY4 = 2},
              Four MDataPoint4 { bX = "Week8", bY1 = 2, bY2 = 4, bY3 = 10, bY4 = 1},
              Four MDataPoint4 { bX = "Week9", bY1 = 0, bY2 = 7, bY3 = 11, bY4 = 0} ]
  }

-- barData :: MultiDataSet
-- barData
--   = MultiDataSet
--   { mLabels = ["1938","1939","1940","1941","1942","1943"]
--   , mSeries = [[12000,9000,7000,8000,12000,10000],[2000,1000,3500,7000,5000,9000]]
--   }

-- pieData :: SingleDataSet
-- pieData
--   = SingleDataSet
--   { sLabels = ["Day one","Day two","Day three","Day four"]
--   , sSeries = [20,15,40,10]
--   }

-- donutData :: SingleDataSet
-- donutData
--   = SingleDataSet
--   { sLabels = []
--   , sSeries = [60,20]
--   }
