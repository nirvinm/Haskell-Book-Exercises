module Main where

import FizzBuzz

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzList [1..100]
