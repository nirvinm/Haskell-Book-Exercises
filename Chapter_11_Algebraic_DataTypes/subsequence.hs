module Subsequence where

-- Use as-patterns in implementing the following functions:
-- This should return True if (and only if) all the values in the
-- first list appear in the second list, though they need not be
-- contiguous.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf ks@(k:ks') (x:xs') =
    if k == x
    then isSubsequenceOf ks' xs'
    else isSubsequenceOf ks  xs'
