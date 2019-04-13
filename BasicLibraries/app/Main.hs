module Main where

import Criterion.Main
import DList


schlemiel :: Int -> [Int]
schlemiel i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) ([n] ++ xs)


constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
    where go 0 xs = xs
          go n xs =
            go (n-1) 
            (singleton n `append` xs)

main :: IO ()
main = defaultMain
    [ bench "concat list" $
        whnf schlemiel 123456
    , bench "concat dlist" $
        whnf constructDlist 123456
    ]

{-
 -    SAMPLE RESULTS
 -            
 -    benchmarking concat list
 -    time                 16.39 ms   (15.04 ms .. 19.29 ms)
 -                         0.904 R²   (0.810 R² .. 0.999 R²)
 -    mean                 15.66 ms   (15.10 ms .. 17.24 ms)
 -    std dev              2.133 ms   (614.3 μs .. 4.171 ms)
 -    variance introduced by outliers: 65% (severely inflated)
 -            
 -    benchmarking concat dlist
 -    time                 5.478 ms   (5.347 ms .. 5.604 ms)
 -                         0.995 R²   (0.991 R² .. 0.998 R²)
 -    mean                 5.799 ms   (5.695 ms .. 6.038 ms)
 -    std dev              428.8 μs   (209.1 μs .. 739.5 μs)
 -    variance introduced by outliers: 45% (moderately inflated)
 -}