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

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
-- What it should do:
-- Prelude> (BoolConj True) <> (BoolConj True)
-- BoolConj True
-- Prelude> (BoolConj True) <> (BoolConj False)
-- BoolConj False
instance Semigroup BoolConj where 
    (BoolConj True) <> (BoolConj True) = (BoolConj True)
    _ <> _ = (BoolConj False)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
-- What it should do:
-- Prelude> (BoolDisj True) <> (BoolDisj True)
-- BoolDisj True
-- Prelude> (BoolDisj True) <> (BoolDisj False)
-- BoolDisj True
instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
    _ <> _ = (BoolDisj False)

data Or a b =
      Fst a
    | Snd b
    deriving (Eq, Show)

-- The Semigroup for Or should have the following behavior. We
-- can think of this as having a “sticky” Snd value where it’ll hold
-- onto the first Snd value when and if one is passed as an argument.
-- This is similar to the First' Monoid you wrote earlier.

-- Prelude> Fst 1 <> Snd 2
-- Snd 2
-- Prelude> Fst 1 <> Fst 2
-- Fst 2
-- Prelude> Snd 1 <> Fst 2
-- Snd 1
-- Prelude> Snd 1 <> Snd 2
-- Snd 1
instance Semigroup (Or a b) where
    (Snd x) <> _       = Snd x
    _       <> (Snd y) = Snd y
    x       <> y       = y

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

-- What it should do:
-- Prelude> let f = Combine $ \n -> Sum (n + 1)
-- Prelude> let g = Combine $ \n -> Sum (n - 1)
-- Prelude> unCombine (f <> g) $ 0
-- Sum {getSum = 0}
-- Prelude> unCombine (f <> g) $ 1
-- Sum {getSum = 2}
-- Prelude> unCombine (f <> f) $ 1
-- Sum {getSum = 4}
-- Prelude> unCombine (g <> f) $ 1
-- Sum {getSum = 2}

-- Hint: This function will eventually be applied to a single value
-- of type 𝑎. But you’ll have multiple functions that can produce a
-- value of type 𝑏. How do we combine multiple values so we have
-- a single 𝑏? This one will probably be tricky! Remember that the
-- type of the value inside of Combine is that of a function. The type
-- of functions should already have an Arbitrary instance that you
-- can reuse for testing this instance.
instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = (Combine $ \x -> f x <> g x)

newtype Comp a =
    Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = (Comp $ \k -> f $ g k)

data Validation a b =
      Failure a
    | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure x) <> (Failure y) = (Failure $ x <> y)
    (Success x) <> _ = (Success x)
    _ <> (Success y) = (Success y)


