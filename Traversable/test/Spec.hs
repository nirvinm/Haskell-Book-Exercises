import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import ChapterExercises

main :: IO ()
main = do
    quickBatch $ traversable (undefined :: Identity (String, Char, [Int]))
    quickBatch $ traversable (undefined :: Constant (String, Char, [Int]) (String, Char, [Int]))
    quickBatch $ traversable (undefined :: Optional (String, Char, [Int]))
    quickBatch $ traversable (undefined :: List (String, Char, [Int]))
    quickBatch $ traversable (undefined :: Three Int Int (String, Char, [Int]))
    quickBatch $ traversable (undefined :: Pair Int (String, Char, [Int]))
    quickBatch $ traversable (undefined :: Big Int (String, Char, [Int]))
