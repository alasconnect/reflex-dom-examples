module Layout (runLayout) where

import Control.Monad.IO.Class (MonadIO(..))
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
  el "h1" $ text "Plotly.js integration with Reflex DOM!"

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
  _ <- perform (const $ makeBarChart barData) =<< delay 0 =<< getPostBuild
  _ <- perform (const $ makePieChart pieData) =<< delay 0 =<< getPostBuild
  _ <- perform (const $ makeDonutChart donutData) =<< delay 0 =<< getPostBuild

  --e <- getPostBuild
  --e' <- delay 0 e
  --_ <- performEvent $ liftIO makeLineChart <$ e'

  return ()

-- Invoke JS FFI (developed by Chris, based on Reflex source code)
perform :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
perform f a = performEvent (fmap (liftIO . f) a)

-- Data to display in charts
red :: String
red = "rgb(223,12,12)"
salmon :: String
salmon = "rgb(249,119,119)"
yellow :: String
yellow = "rgb(255,239,0)"
brown :: String
brown = "rgb(255,179,0)"

lineLabels :: [String]
lineLabels = ["Week1","Week2","Week3","Week4","Week5","Week6","Week7","Week8","Week9"]
lineData :: [MultiDataSet]
lineData
  = [ MultiDataSet { dX = lineLabels, dY = [12,9,7,8,6,4,3,2,0], dType = "", dMode = "lines+markers",
                     dOffset = 0, dLine = Line { lColor = red, lWidth = 3 },
                     dMarker = MMarker { mColor = red } },
      MultiDataSet { dX = lineLabels, dY = [2,1,3.5,7,9,8,7.7,4,7], dType = "", dMode = "lines+markers",
                     dOffset = 0, dLine = Line { lColor = salmon, lWidth = 3 },
                     dMarker = MMarker { mColor = salmon } },
      MultiDataSet { dX = lineLabels, dY = [1,3,4,5,6,8,9,10,11], dType = "", dMode = "lines+markers",
                     dOffset = 0,dLine = Line { lColor = yellow, lWidth = 3 },
                     dMarker = MMarker { mColor = yellow } },
      MultiDataSet { dX = lineLabels, dY = [11,7.5,5.5,5.5,4,3.5,2,1,0], dType = "", dMode = "lines+markers",
                     dOffset = 0, dLine = Line { lColor = brown, lWidth = 3 },
                     dMarker = MMarker { mColor = brown } } ]

barLabels :: [String]
barLabels = ["1938","1939","1940","1941","1942","1943"]
barData :: [MultiDataSet]
barData
  = [ MultiDataSet { dX = barLabels, dY = [12000,9000,7000,8000,12000,10000], dType = "bar", dMode = "", dOffset = -0.1,
                 dLine = Line { lColor = red, lWidth = 0 }, dMarker = MMarker { mColor = red } },
      MultiDataSet { dX = barLabels, dY = [2000,1000,3500,7000,5000,9000], dType = "bar", dMode = "", dOffset = 0.1,
                 dLine = Line { lColor = salmon, lWidth = 0 }, dMarker = MMarker { mColor = salmon } } ]

pieData :: [SingleDataSet]
pieData
  = [ SingleDataSet { sLabels = ["Day one","Day two","Day three","Day four"], sValues = [20,15,40,10], sType = "pie",
                      sHole = 0.0, sText = [], sTextinfo = "label", sTextposition = "outside", sSort = False,
                      sDirection = "clockwise", sMarker = SMarker { sColors = [red, salmon, yellow, brown] } } ]

donutData :: [SingleDataSet]
donutData
  = [ SingleDataSet { sLabels = [], sValues = [60,20,20], sType = "pie", sHole = 0.6, sText = ["60","20"],
                      sTextinfo = "text", sTextposition = "inside",  sSort = True, sDirection = "clockwise",
                      sMarker = SMarker { sColors = [red, salmon, "rgb(255,255,255)"] } } ]
