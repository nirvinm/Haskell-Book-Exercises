module Main where

import Criterion.Main
import Data.Maybe
import qualified Data.Sequence as DS
import Debug.Trace

import DList
import Queue

-- Bechmarks for Difference List
schlemiel :: Int -> [Int]
schlemiel i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) ([n] ++ xs)


constructDlist :: Int -> [Int]
constructDlist i = toList $ go i DList.empty
    where go 0 xs = xs
          go n xs =
            go (n-1) 
            (singleton n `append` xs)


-- Benchmarks for Queue
seqPush :: Int -> DS.Seq Int
seqPush i = foldl (DS.|>) DS.empty [1..i]

queuePush :: Int -> Queue Int
queuePush i = foldl (flip push) Queue.empty [1..i]

seqPop :: Int -> DS.Seq Int
seqPop i =
    foldl go (DS.fromList [1..i]) [1..i]
    where go seq _ = DS.deleteAt 0 seq

queuePop :: Int -> Queue Int
queuePop i =
    foldl go (Queue.fromList [1..i]) [1..i]
    where go q _ = snd $ fromMaybe (0, Queue.empty) $ pop q


main :: IO ()
main = defaultMain
    [ bench "concat list" $
        whnf schlemiel 123456
    , bench "concat dlist" $
        whnf constructDlist 123456
    , bench "sequence push" $
        whnf seqPush 123456
    , bench "queue push" $
        whnf queuePush 123456
    , bench "sequence pop" $
        whnf seqPop 123456
    , bench "queue pop" $
        whnf queuePop 123456
    ]

{-
 - SAMPLE RESULTS
 -            
 - benchmarking concat list
 - time                 15.88 ms   (15.09 ms .. 17.05 ms)
 -                      0.978 R²   (0.949 R² .. 0.999 R²)
 - mean                 15.18 ms   (14.91 ms .. 15.82 ms)
 - std dev              1.026 ms   (448.8 μs .. 1.847 ms)
 - variance introduced by outliers: 32% (moderately inflated)
 - 
 - benchmarking concat dlist
 - time                 5.462 ms   (5.404 ms .. 5.519 ms)
 -                      0.999 R²   (0.999 R² .. 1.000 R²)
 - mean                 5.544 ms   (5.519 ms .. 5.585 ms)
 - std dev              93.86 μs   (71.09 μs .. 132.9 μs)
 - 
 - benchmarking sequence push
 - time                 18.46 ms   (18.13 ms .. 18.77 ms)
 -                      0.998 R²   (0.994 R² .. 1.000 R²)
 - mean                 18.67 ms   (18.49 ms .. 19.24 ms)
 - std dev              726.1 μs   (191.1 μs .. 1.417 ms)
 - variance introduced by outliers: 13% (moderately inflated)
 - 
 - benchmarking queue push
 - time                 14.88 ms   (14.70 ms .. 15.06 ms)
 -                      0.999 R²   (0.999 R² .. 1.000 R²)
 - mean                 14.69 ms   (14.60 ms .. 14.79 ms)
 - std dev              233.2 μs   (175.6 μs .. 341.6 μs)
 - 
 - benchmarking sequence pop
 - time                 20.93 ms   (20.25 ms .. 21.42 ms)
 -                      0.997 R²   (0.994 R² .. 0.999 R²)
 - mean                 23.41 ms   (22.50 ms .. 24.79 ms)
 - std dev              2.580 ms   (1.637 ms .. 3.554 ms)
 - variance introduced by outliers: 48% (moderately inflated)
 - 
 - benchmarking queue pop
 - time                 2.992 ms   (2.738 ms .. 3.140 ms)
 -                      0.955 R²   (0.854 R² .. 0.998 R²)
 - mean                 3.963 ms   (3.449 ms .. 6.218 ms)
 - std dev              3.057 ms   (561.1 μs .. 6.898 ms)
 - variance introduced by outliers: 98% (severely inflated)
 -}