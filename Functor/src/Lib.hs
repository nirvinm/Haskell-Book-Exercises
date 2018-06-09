{-# LANGUAGE FlexibleInstances #-}

module Lib where

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity  where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a $ f b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b $ f c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

-- Can you implement one for this type? Why? Why not?
--   Since the kind of this type is * and not * -> *,
--   a Functor instance cannot be defined.
data Trivial = Trivial


-- Write a Functor instance for a datatype identical to Maybe. We’ll use
-- our own datatype because Maybe already has a Functor instance and
-- we cannot make a duplicate one.
data Possibly a =
      LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap f LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- 1. Write a Functor instance for a datatype identical to Either. We’ll
--    use our own datatype because Either has a Functor instance.
data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

-- Rearrange the arguments to the type constructor of the datatype
-- so the Functor instance works.

-- 1.
data Sum' a b =
      First' b
    | Second' a

instance Functor (Sum' e) where
    fmap f (First' b) = First' (f b)
    fmap f (Second' a) = Second' a

-- 2.
data Company a b c =
      DeepBlue a b
    | Something c

instance Functor (Company e e') where
    fmap f (Something c) = Something (f c)
    fmap _ (DeepBlue a b) = DeepBlue a b

-- 3.
data More a b =
      L b a b
    | R a b a
    deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L b a b') = L (f b) a (f b')
    fmap f (R a b a') = R a (f b) a'

-- Write Functor instances for the following datatypes.
-- 1.
data Quant a b =
      Finance
    | Desk a
    | Bloor b

instance Functor (Quant x) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- 2. No, it’s not interesting by itself.
data K a b =
    K a

instance Functor (K a) where
    fmap _ (K a) = K a

-- 3.
newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K' a b =
    K' a

instance Functor (Flip K' x) where
    fmap f (Flip (K' a)) = Flip (K' (f a))

-- should remind you of an
-- instance you've written before
-- instance Functor (Flip K' a) where
--     fmap f (Flip K' a b) = Flip $ K' (f a) b

-- 4.
data EvilGoateeConst a b =
    GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 5. Do you need something extra to make the instance work?
data LiftItOut f a =
    LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
    fmap fn (LiftItOut fa) = LiftItOut $ fmap fn fa

-- 6.
data Parappa f g a =
    DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap fn (DaWrappa fa ga) = DaWrappa (fmap fn fa) (fmap fn ga)

-- 7. Don’t ask for more typeclass instances than you need. You can
-- let GHC tell you what to do.
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap fn (IgnoringSomething fa gb) = IgnoringSomething fa (fmap fn gb)

-- 8.
data Notorious g o a t =
    Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9. You’ll need to use recursion.
data List a =
      Nil
    | Cons a (List a)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a ls) = Cons (f a) (fmap f ls)

-- 10. A tree of goats forms a Goat-Lord, fearsome poly-creature.
data GoatLord a =
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a)
                (GoatLord a)
                (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11. You’ll use an extra functor for this one, although your solution
-- might do it monomorphically without using fmap. Keep in
-- mind that you will probably not be able to validate this one in
-- the usual manner. Do your best to make it work.
data TalkToMe a =
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print x a) = Print x (f a)
    fmap f (Read fn) = Read (f.fn)
