import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import ChapterExercises

main :: IO ()
main = do
    quickBatch $ traversable (undefined :: Identity (String, Char, [Int]))