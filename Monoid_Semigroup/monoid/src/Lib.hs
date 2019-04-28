-- Given a datatype, implement the Monoid instance. Add Monoid constraints
-- to type variables where needed. For the datatypes you’ve
-- already implemented Semigroup instances for, you need to figure out
-- what the identity value is.

module Lib where

import Data.Monoid

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _= Trivial

instance Monoid Trivial where 
    mempty = Trivial

type TrivialAssoc =
    Trivial -> Trivial -> Trivial -> Bool


newtype Identity a = Identity a deriving (Eq, Show)


instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty = Two mempty mempty


newtype BoolConj = BoolConj Bool deriving (Eq, Show)


instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _               <> _               = BoolConj False

instance Monoid BoolConj where
    mempty = BoolConj True

    
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _               = BoolDisj True
    _               <> (BoolDisj True) = BoolDisj True
    _               <> _                = BoolDisj False

instance Monoid BoolDisj where
    mempty = BoolDisj False


newtype Combine a b = 
    Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where 
    (Combine f) <> (Combine g) = Combine $ \k -> ((f k) <> (g k))

instance (Monoid b) => Monoid (Combine a b) where 
    mempty = Combine mempty


newtype Comp a = Comp (a -> a)

instance (Semigroup a) => Semigroup (Comp a) where 
    (Comp f) <> (Comp g) = Comp $ \k -> f $ g k

instance (Monoid a) => Monoid (Comp a) where 
    mempty = Comp mempty


newtype Mem s a =
    Mem {
        runMem :: s -> (a,s)
    }


instance Semigroup a => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) =
        Mem $ \k -> 
            let (a, s) = f k
                (a', s') = g k
            in (a <> a', s')

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \k -> (mempty, k)
