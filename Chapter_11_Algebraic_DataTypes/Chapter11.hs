module Chapter11 where 

import Data.Char

-- Use as-patterns in implementing the following functions:
-- 1. This should return True if (and only if) all the values in the
--    first list appear in the second list, though they need not be
--    contiguous.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf ks [] = False
isSubsequenceOf ks@(k:ks') xs@(x:xs') =
    if k == x
    then isSubsequenceOf ks' xs'
    else isSubsequenceOf ks  xs'

-- 2. Split a sentence into words, then tuple each word with the capi-
--    talized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map (\x -> (x, capitalizeWord x)) $ words xs

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x) : xs
