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
  el "h1" $ text "Chart.js integration with Reflex DOM!"

  elAttr "ul" ("style"=:"list-style-type:none;min-width:600px") $ do
    elAttr "li" ("style"=:"width:49%;float:left") $
      elAttr "canvas" ("id"=:"topLeft") blank
    elAttr "li" ("style"=:"width:49%;float:left;clear:right") $
      elAttr "canvas" ("id"=:"topRight") blank
    elAttr "li" ("style"=:"width:49%;float:left") $
      elAttr "canvas" ("id"=:"bottomLeft") blank
    elAttr "li" ("style"=:"width:49%;float:left;clear:right") $
      elAttr "canvas" ("id"=:"bottomRight") blank

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
red :: String
red = "rgba(223,12,12,1)"
salmon :: String
salmon = "rgba(249,119,119,1)"
yellow :: String
yellow = "rgba(255,239,0,1)"
brown :: String
brown = "rgba(255,179,0,1)"

lineData :: MultiDataSet
lineData
  = MultiDataSet
  { mLabels = ["Week1","Week2","Week3","Week4","Week5","Week6","Week7","Week8","Week9"]
  , mDatasets = [ MDataset { mData = [12,9,7,8,6,4,3,2,0], mFill = False,
                             mBorderColor = red, mBackgroundColor = red },
                  MDataset { mData = [2,1,3.5,7,9,8,7.7,4,7], mFill = False,
                             mBorderColor = salmon, mBackgroundColor = salmon },
                  MDataset { mData = [1,3,4,5,6,8,9,10,11], mFill = False,
                             mBorderColor = yellow, mBackgroundColor = yellow },
                  MDataset { mData = [11,7.5,5.5,5.5,4,3.5,2,1,0], mFill = False,
                             mBorderColor = brown, mBackgroundColor = brown } ]
  }

barData :: MultiDataSet
barData
  = MultiDataSet
  { mLabels = ["1938","1939","1940","1941","1942","1943"]
  , mDatasets = [ MDataset { mData = [12000,9000,7000,8000,12000,10000], mFill = True,
                             mBorderColor = red, mBackgroundColor = red },
                  MDataset { mData = [2000,1000,3500,7000,5000,9000], mFill = True,
                             mBorderColor = salmon, mBackgroundColor = salmon } ]
  }

pieData :: SingleDataSet
pieData
  = SingleDataSet
  { sLabels = ["Day one","Day two","Day three","Day four"]
  , sDatasets = [ SDataset { sData = [20,15,40,10], sBorderColor = [red, salmon, yellow, brown],
                             sBackgroundColor = [red, salmon, yellow, brown] } ]

  }

donutData :: SingleDataSet
donutData
  = SingleDataSet
  { sLabels = []
  , sDatasets = [ SDataset { sData = [60,20], sBorderColor = [red, salmon], sBackgroundColor = [red, salmon] } ]
  }
