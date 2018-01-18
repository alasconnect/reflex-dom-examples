{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, DeriveGeneric #-}

module JavascriptTools where

import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Aeson 
  ( FromJSON, ToJSON
  , encode )
import Data.JSString
  ( JSString
  , pack )
import GHC.Generics

-- Data structure that embodies the data sent to the Chartist functions
data ChartData = ChartData { labels :: [String]
                           , series :: [[Int]]
                           } deriving (Generic, Show)
instance ToJSON ChartData

-- Converts a ChartData type to a string, in the correct JSON form
toJsString :: ChartData -> Data.JSString.JSString
toJsString cd = Data.JSString.pack $ show $ encode cd

-- Create a line chart using Chartist
makeLineChart :: MonadIO m => ChartData -> m ()
makeLineChart cd = liftIO $ jsMakeLineChart $ toJsString cd

foreign import javascript unsafe 
  "new Chartist.Line('#topLeft', JSON.parse($1));"
  jsMakeLineChart :: Data.JSString.JSString -> IO ()



-- Create a bar chart using Chartist
--TODO



-- Create a pie chart using Chartist
--TODO



-- Create a donut chart using Chartist
--TODO