import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import EitherMonad
import ChapterExercises

main :: IO ()
main = do
    quickBatch $ monad (undefined :: Sum Int (String, Char, Integer))
    quickBatch $ monad (undefined :: Nope (String, Char, Integer))
    quickBatch $ monad (undefined :: PhhhbbtttEither Int (String, Char, Integer))
    quickBatch $ monad (undefined :: Identity (String, Char, Integer))
    quickBatch $ monad (undefined :: List (String, Char, Integer))