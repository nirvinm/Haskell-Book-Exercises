module Lib where

import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)
        
instance Semigroup Trivial where
    _ <> _ = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

