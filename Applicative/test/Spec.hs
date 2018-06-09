import Test.QuickCheck hiding (Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import ListApplicative
import Validation

main :: IO()
main = do
    quickBatch $ functor (undefined :: List (String, Char, Integer))
    quickBatch $ applicative (undefined :: List (String, Char, Integer))
    quickBatch $ functor (undefined :: ZipList (String, Char, Integer))
    quickBatch $ applicative (undefined :: ZipList (String, Char, Integer))
    quickBatch $ functor (undefined :: Validation Int (String, Char, Int))
    quickBatch $ applicative (undefined :: Validation String (String, Char, Int))
    
