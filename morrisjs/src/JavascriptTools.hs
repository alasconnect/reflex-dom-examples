{-# LANGUAGE
    DeriveGeneric
  , ForeignFunctionInterface
  , JavaScriptFFI
  , TemplateHaskell
#-}

module JavascriptTools
  ( MDataPoint(..)
  , MDataPoint2(..)
  , MDataPoint4(..)
  , MultiDataSet(..)
  , SDataPoint(..)
  , SingleDataSet(..)
  , makeBarChart
  , makeDonutChart
  , makeLineChart
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
  ( defaultOptions
  , encode
  )
import qualified Data.Aeson.TH as ATH
  ( fieldLabelModifier
  , deriveJSON )
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Data.JSString as Js
  ( JSString
  , pack
  )
import GHC.Generics
import Utils

-- Internal structure for each data point within SingleDataSet
data SDataPoint
  = SDataPoint
  { sLabel :: String
  , sValue :: Double
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SDataPoint

-- Data structure that Morris.js uses for charts with just one set of data
data SingleDataSet
  = SingleDataSet
  { sData :: [SDataPoint]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SingleDataSet

-- Internal structure for each data point within MultiDataSet
data MDataPoint2
  = MDataPoint2
  { aX :: String
  , aY1 :: Double
  , aY2 :: Double
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MDataPoint2

-- Internal structure for each data point within MultiDataSet
data MDataPoint4
  = MDataPoint4
  { bX :: String
  , bY1 :: Double
  , bY2 :: Double
  , bY3 :: Double
  , bY4 :: Double
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MDataPoint4

-- Wrapper for either a MDataPoint2 or MDataPoint4, used by MultiDataSet to allow different dataset sizes
data MDataPoint = Two MDataPoint2 | Four MDataPoint4 deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 0 } ''MDataPoint

-- Data structure that Morris.js uses for charts with multiple sets of data
data MultiDataSet
  = MultiDataSet
  { mData :: [MDataPoint]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MultiDataSet

-- Wrapper for either a SingleDataSet or MultiDataSet, used by toJsString helper function
data DataSet = Single SingleDataSet | Multi MultiDataSet

-- Encode a _DataSet type to JSON, converts that to a String, and then to a JSString
toJsString :: DataSet -> Js.JSString
toJsString d =
  case d of
    Single s -> Js.pack . Bs.unpack . encode $ s
    Multi  m -> Js.pack . Bs.unpack . encode $ m

----------------------------------------
-- Create a LINE chart using Chartist.js
makeLineChart :: MonadIO m => MultiDataSet -> m ()
makeLineChart = liftIO . jsMakeLineChart . toJsString . Multi

foreign import javascript unsafe
  "console.log($1);var obj = JSON.parse($1);\
  \new Morris.Line( { element:'topLeft', data:obj.data, xkey:'x', ykeys:['y1', 'y2'] } );"
  jsMakeLineChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a BAR chart using Chartist.js
makeBarChart :: MonadIO m => MultiDataSet -> m ()
makeBarChart = liftIO . jsMakeBarChart . toJsString . Multi

foreign import javascript unsafe
  "var responsiveOptions = [ ['screen and (min-width:1000px)', { seriesBarDistance:18 } ] ];\
  \new Chartist.Bar( '#topRight', JSON.parse($1), { seriesBarDistance:6 }, responsiveOptions );"
  jsMakeBarChart :: Js.JSString -> IO ()

-----------------------------------------
-- Create a DONUT chart using Chartist.js
makeDonutChart :: MonadIO m => SingleDataSet -> m ()
makeDonutChart = liftIO . jsMakeDonutChart . toJsString . Single

foreign import javascript unsafe
  "var options = { donut:true, donutWidth:50, total:100, chartPadding:20, labelOffset:-60,\
  \                labelInterpolationFnc: function (val) { return val + '%'; } };\
  \var responsiveOptions = [ ['screen and (max-width:1000px)', { donutWidth:30, labelOffset:40 } ] ];\
  \new Chartist.Pie( '#bottomRight', JSON.parse($1), options, responsiveOptions );"
  jsMakeDonutChart :: Js.JSString -> IO ()
