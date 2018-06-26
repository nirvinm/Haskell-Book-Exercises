module ChapterExercises where

import Data.Monoid
import Control.Applicative

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
                 


--2. This should look familiar.
data Two a b = Two a b deriving (Eq, Show)


instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
    pure = Two mempty
    (Two x f) <*> (Two x' y) = Two (x <> x') (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
                    x <- arbitrary
                    y <- arbitrary
                    return $ Two  x y

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq




    -- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (Three x y f) <*> (Three x' y' z') = Three (x <> x') (y <> y') (f z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
                    x <- arbitrary
                    y <- arbitrary
                    z <- arbitrary
                    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq




-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' a f g)  <*> (Three' a' x y) = Three' (a <> a') (f x) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
                    x <- arbitrary
                    y <- arbitrary
                    z <- arbitrary
                    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq



-- Combinations
-- Remember the vowels and stops exercise in the folds chapter? Write
-- the function to generate the possible combinations of three input
-- lists using liftA3 from Control.Applicative.
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
