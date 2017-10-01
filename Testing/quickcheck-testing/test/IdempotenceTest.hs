module IdempotenceTest where 

import Data.List
import Lib

-- Use QuickCheck and the following helper functions to demonstrate
-- idempotence for the following:
twice f = f . f
fourTimes = twice . twice

-- 1. 
f :: String -> Bool
f x =
    (capitalizeWord x == twice capitalizeWord x)
    &&
    (capitalizeWord x == fourTimes capitalizeWord x)

f' :: [String] -> Bool
f' x =
    (sort x == twice sort x)
    &&
    (sort x == fourTimes sort x)

