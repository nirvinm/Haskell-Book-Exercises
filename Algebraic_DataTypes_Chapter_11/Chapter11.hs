module Chapter11 where 

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
