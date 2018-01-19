{-# LANGUAGE 
    DeriveGeneric
  , ForeignFunctionInterface
  , JavaScriptFFI
  , TemplateHaskell
#-}

module JavascriptTools 
  ( MultiDataSet(..)
  , SingleDataSet(..)
  , makeBarChart
  , makeDonutChart
  , makeLineChart
  , makePieChart
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

-- Data structure that Chartist.js uses for charts with just one set of data
data SingleDataSet
  = SingleDataSet 
  { sLabels :: [String]
  , sSeries :: [Double]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SingleDataSet

-- Data structure that Chartist.js uses for charts with multiple sets of data
data MultiDataSet
  = MultiDataSet 
  { mLabels :: [String]
  , mSeries :: [[Double]]
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
  "var responsiveOptions = [ ['screen and (max-width:1000px)', { showPoint:false, axisX:\
  \                        { labelInterpolationFnc: function(val) { return val[0] + val[4]; } } } ] ];\
  \new Chartist.Line( '#topLeft', JSON.parse($1), null, responsiveOptions );"
  jsMakeLineChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a BAR chart using Chartist.js 
makeBarChart :: MonadIO m => MultiDataSet -> m ()
makeBarChart = liftIO . jsMakeBarChart . toJsString . Multi

foreign import javascript unsafe 
  "var responsiveOptions = [ ['screen and (min-width:1000px)', { seriesBarDistance:18 } ] ];\
  \new Chartist.Bar( '#topRight', JSON.parse($1), { seriesBarDistance:6 }, responsiveOptions );"
  jsMakeBarChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a PIE chart using Chartist.js
makePieChart :: MonadIO m => SingleDataSet -> m ()
makePieChart = liftIO . jsMakePieChart . toJsString. Single

foreign import javascript unsafe 
  "var options = { chartPadding:20 };\
  \var responsiveOptions = [ ['screen and (max-width:1200px)', { labelPosition:'outside', labelOffset:20,\
  \                           labelInterpolationFnc: function(val) { return (val.split(' '))[1]; } } ] ];\
  \new Chartist.Pie( '#bottomLeft', JSON.parse($1), options, responsiveOptions );"
  jsMakePieChart :: Js.JSString -> IO ()

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
