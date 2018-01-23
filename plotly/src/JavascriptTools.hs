{-# LANGUAGE
    DeriveGeneric
  , ForeignFunctionInterface
  , JavaScriptFFI
  , TemplateHaskell
#-}

module JavascriptTools
  ( Line(..)
  , MMarker(..)
  , SMarker(..)
  , MultiDataSet(..)
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

-- Data required specifically for pie/donut charts
data SMarker
  = SMarker
  { sColors :: [String]
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SMarker

-- Data structure that Plotly.js uses for charts with just one set of data
data SingleDataSet
  = SingleDataSet
  { sLabels :: [String]
  , sValues :: [Double]
  , sType :: String
  , sHole :: Double
  , sText :: [String]
  , sTextinfo :: String
  , sTextposition :: String
  , sSort :: Bool
  , sDirection :: String
  , sMarker :: SMarker
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''SingleDataSet

-- Data required specifically for bar charts
data MMarker
  = MMarker
  { mColor :: String
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MMarker

-- Data required specifically for line charts
data Line
  = Line
  { lColor :: String
  , lWidth :: Int
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''Line

-- Data structure that Plotly.js uses for charts with multiple sets of data
data MultiDataSet
  = MultiDataSet
  { dX :: [String]
  , dY :: [Double]
  , dType :: String -- mainly for bar
  , dMode :: String -- mainly for line
  , dOffset :: Double -- mainly for bar
  , dLine :: Line
  , dMarker :: MMarker
  } deriving (Show, Generic)
ATH.deriveJSON defaultOptions { ATH.fieldLabelModifier = custFieldLabelModifier 1 } ''MultiDataSet

-- Wrapper for either a SingleDataSet or MultiDataSet, used by toJsString helper function
data DataSet = Single [SingleDataSet] | Multi [MultiDataSet]

-- Encode a _DataSet type to JSON, converts that to a String, and then to a JSString
toJsString :: DataSet -> Js.JSString
toJsString d =
  case d of
    Single s -> Js.pack . Bs.unpack . encode $ s
    Multi  m -> Js.pack . Bs.unpack . encode $ m

----------------------------------------
-- Create a LINE chart using Plotly.js
makeLineChart :: MonadIO m => [MultiDataSet] -> m ()
makeLineChart = liftIO . jsMakeLineChart . toJsString . Multi

foreign import javascript unsafe
  "Plotly.newPlot( 'topLeft', JSON.parse($1), { showlegend:false, yaxis: { zeroline:false } } );"
  jsMakeLineChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a BAR chart using Plotly.js
makeBarChart :: MonadIO m => [MultiDataSet] -> m ()
makeBarChart = liftIO . jsMakeBarChart . toJsString . Multi

foreign import javascript unsafe
  "Plotly.newPlot( 'topRight', JSON.parse($1), { barmode:'group', bargap:0.7,\
  \showlegend:false, yaxis: { zeroline:false } } );"
  jsMakeBarChart :: Js.JSString -> IO ()

---------------------------------------
-- Create a PIE chart using Plotly.js
makePieChart :: MonadIO m => [SingleDataSet] -> m ()
makePieChart = liftIO . jsMakePieChart . toJsString. Single

foreign import javascript unsafe
  "Plotly.newPlot( 'bottomLeft', JSON.parse($1), { showlegend:false } );"
  jsMakePieChart :: Js.JSString -> IO ()

-----------------------------------------
-- Create a DONUT chart using Plotly.js
makeDonutChart :: MonadIO m => [SingleDataSet] -> m ()
makeDonutChart = liftIO . jsMakeDonutChart . toJsString . Single

foreign import javascript unsafe
  "Plotly.newPlot( 'bottomRight', JSON.parse($1), { showlegend:false } );"
  jsMakeDonutChart :: Js.JSString -> IO ()
