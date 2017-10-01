module Lib where

import Data.Char

half :: Fractional a => a -> a
half x = x / 2

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (c:cs) = toUpper c : cs
