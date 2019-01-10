module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 5 == 0 = "Buzz"
    | n `mod` 3 == 0 = "Fizz"
    | otherwise = show n


addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)


fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM addResult list) []

-- Fizzbuzz Differently
-- It’s an exercise! Rather than changing the underlying data structure,
-- fix our reversing fizzbuzz by changing the code in the following way:
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo begin end = fizzbuzzList [end,(end-1)..begin]