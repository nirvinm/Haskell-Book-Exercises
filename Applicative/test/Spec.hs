import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import ListApplicative
import Validation
import ChapterExercises

main :: IO()
main = do
    quickBatch $ functor (undefined :: List (String, Char, Integer))
    quickBatch $ applicative (undefined :: List (String, Char, Integer))
    quickBatch $ functor (undefined :: ZipList (String, Char, Integer))
    quickBatch $ applicative (undefined :: ZipList (String, Char, Integer))
    quickBatch $ functor (undefined :: Validation Int (String, Char, Int))
    quickBatch $ applicative (undefined :: Validation String (String, Char, Int))
    quickBatch $ functor (undefined :: Pair (String, Char, Int))
    quickBatch $ applicative (undefined :: Pair (String, Char, Int))
    quickBatch $ functor (undefined :: Two String (String, Char, Int))
    quickBatch $ applicative (undefined :: Two String (String, Char, Int))
    quickBatch $ functor (undefined :: Three String String (String, Char, Int))
    quickBatch $ applicative (undefined :: Three String String (String, Char, Int))
    quickBatch $ functor (undefined :: Three' String (String, Char, Int))
    quickBatch $ applicative (undefined :: Three' String (String, Char, Int))
