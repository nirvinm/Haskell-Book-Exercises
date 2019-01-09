import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

import Moi

main :: IO ()
main = do
    quickBatch $ functor (undefined :: (Moi Int (Int, Int, Int)))
    quickBatch $ applicative (undefined :: (Moi Int (Int, Int, Int)))
    quickBatch $ monad (undefined :: (Moi Int (Int, Int, Int)))