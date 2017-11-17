-- Given a datatype, implement the Monoid instance. Add Monoid constraints
-- to type variables where needed. For the datatypes you’ve
-- already implemented Semigroup instances for, you need to figure out
-- what the identity value is.

module Lib where

import Data.Monoid

data Trivial = Trivial deriving (Eq, Show)

instance Monoid Trivial where 
    mempty = Trivial
    mappend _ _= Trivial

type TrivialAssoc =
    Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty = Two mempty mempty
    mappend (Two x y) (Two x' y') = Two (mappend x x') (mappend y y')

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend (BoolConj True) (BoolConj True) = BoolConj True
    mappend _ _ = BoolConj False

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend (BoolDisj True) _ = BoolDisj True
    mappend _ (BoolDisj True) = BoolDisj True
    mappend _ _ = BoolDisj False

newtype Combine a b = 
    Combine { unCombine :: (a -> b) }

instance (Monoid b) => Monoid (Combine a b) where 
    mempty = Combine mempty
    mappend (Combine f) (Combine g) = Combine $ \k -> ((f k) `mappend` (g k))

newtype Comp a = Comp (a -> a)

instance (Monoid a) => Monoid (Comp a) where 
    mempty = Comp mempty
    mappend (Comp f) (Comp g) = Comp $ \k -> f $ g k

newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \k -> (mempty, k)
    mappend (Mem f) (Mem g) =
        Mem $ \k -> 
            let (a, s) = f k
                (a', s') = g k
            in (a `mappend` a', s')

