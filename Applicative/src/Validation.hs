module Validation where

import Data.Monoid
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

-- same as Either
instance Functor (Validation e) where
  fmap f x = eitherToValid $ f <$> validToEither x

-- This is different
instance Applicative (Validation e) where
  pure = Success
  (Failure x) <*> _ = Failure x
  _ <*> (Failure y) = Failure y
  (Success f) <*> (Success x) = Success $ f x

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation e a) where
  arbitrary = oneof [ Failure <$> arbitrary,
                      Success <$> arbitrary ]
