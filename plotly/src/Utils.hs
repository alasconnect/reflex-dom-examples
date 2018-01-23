module Utils where

import Data.Char

-- Copy of Models.Common.custFieldLabelModifier from myMTA project
custFieldLabelModifier :: Int -> String -> String
custFieldLabelModifier i = f . drop i
  where
    f [] = [] -- empty case
    f [c] = [Data.Char.toLower c] -- just 1 Char case
    f (x:xs) = Data.Char.toLower x : xs
