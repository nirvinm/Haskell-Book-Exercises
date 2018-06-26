module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Write instances for the following datatypes. Confused? Write
-- out what the type should be. Use the checkers library to validate the
-- instances.

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
                 x <- arbitrary
                 y <- arbitrary
                 return $ Pair x y
                 

