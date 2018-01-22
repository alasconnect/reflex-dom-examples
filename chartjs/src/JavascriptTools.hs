{-# LANGUAGE
    DeriveGeneric
  , ForeignFunctionInterface
  , JavaScriptFFI
  , TemplateHaskell #-}

module JavascriptTools
  ( MDataset(..)
  , MultiDataSet(..)
  , SDataset(..)
  , SingleDataSet(..)
  , makeBarChart
  , makeDonutChart
  , makeLineChart
  , makePieChart
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
  ( defaultOptions
  , encode )
import qualified Data.Aeson.TH as ATH
  ( fieldLabelModifier
  , deriveJSON )
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Data.JSString as Js
  ( JSString
  , pack )
import GHC.Generics
import Utils

-- Internal structure for the the dataset within SingleDataSet
data SDataset
  = SDataset
  { sData :: [Double]
  , sBorderColor :: [String]
  , sBackgroundColor :: [String]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SDataset

-- Data structure that Chart.js uses for charts with just one set of data
data SingleDataSet
  = SingleDataSet
  { sLabels :: [String]
  , sDatasets :: [SDataset]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SingleDataSet

-- Internal structure for the each dataset within MultiDataSet
data MDataset
  = MDataset
  { mData :: [Double]
  , mFill :: Bool
  , mBorderColor :: String
  , mBackgroundColor :: String
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MDataset

-- Data structure that Chart.js uses for charts with multiple sets of data
data MultiDataSet
  = MultiDataSet
  { mLabels :: [String]
  , mDatasets :: [MDataset]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MultiDataSet

-- Wrapper for either a SingleDataSet or MultiDataSet, used by toJsString helper function
data DataSet = Single SingleDataSet | Multi MultiDataSet

-- Encode a ChartData type to JSON, converts that to a String, and then to a JSString
toJsString :: DataSet -> Js.JSString
toJsString d =
  case d of
    Single s -> Js.pack . Bs.unpack . encode $ s
    Multi  m -> Js.pack . Bs.unpack . encode $ m

----------------------------------------
-- Create a LINE chart using Chart.js
makeLineChart :: MonadIO m => MultiDataSet -> m ()
makeLineChart = liftIO . jsMakeLineChart . toJsString . Multi

foreign import javascript unsafe
  "var ctx = document.getElementById('topLeft').getContext('2d');\
  \new Chart( ctx, { type:'line', data:JSON.parse($1), options:{ responsive:true } } );"
  jsMakeLineChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a BAR chart using Chart.js
makeBarChart :: MonadIO m => MultiDataSet -> m ()
makeBarChart = liftIO . jsMakeBarChart . toJsString . Multi

foreign import javascript unsafe
  "var ctx = document.getElementById('topRight').getContext('2d');\
  \new Chart( ctx, { type:'bar', data:JSON.parse($1), options:{ responsive:true } } );"
  jsMakeBarChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a PIE chart using Chart.js
makePieChart :: MonadIO m => SingleDataSet -> m ()
makePieChart = liftIO . jsMakePieChart . toJsString . Single

foreign import javascript unsafe
  "var ctx = document.getElementById('bottomLeft').getContext('2d');\
  \new Chart( ctx, { type:'pie', data:JSON.parse($1), options: { responsive:true } } );"
  jsMakePieChart :: Js.JSString -> IO ()

-----------------------------------------
-- Create a DONUT chart using Chart.js
makeDonutChart :: MonadIO m => SingleDataSet -> m ()
makeDonutChart = liftIO . jsMakeDonutChart . toJsString . Single

foreign import javascript unsafe
  "var ctx = document.getElementById('bottomRight').getContext('2d');\
  \var data = JSON.parse($1); function getSum(total, i) { return total + i; };\
  \function getCirc(arr) { return ((arr.datasets[0].data.reduce(getSum) / 100) * 2 * Math.PI)};\
  \new Chart( ctx, { type:'doughnut', data:data, options: { responsive:true, circumference:getCirc(data) } } );"
  jsMakeDonutChart :: Js.JSString -> IO ()
