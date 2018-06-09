module ListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List a =
      Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance  Monoid (List a) where
    mempty = Nil
    mappend Nil ys = ys
    mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x ls) = Cons (f x) (fmap f ls)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f ls) <*> xs = (fmap f xs) `mappend` (ls <*> xs)

instance Eq a => EqProp (List a) where
    (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = oneof [ pure Nil
                      , Cons <$> arbitrary <*> arbitrary ]

newtype ZipList a =
    ZipList (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

instance Eq a => EqProp (ZipList a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList l) = xs
                    in take' 3000 l
              ys' = let (ZipList l) = ys
                    in take' 3000 l

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
    pure = ZipList . repeat'
    (<*>) (ZipList Nil) _ = ZipList Nil
    (<*>) _ (ZipList Nil) = ZipList Nil
    (<*>) (ZipList (Cons f fs)) (ZipList (Cons y ys)) =
                ZipList $ Cons (f y) rest where
                    (ZipList rest) = ZipList fs <*> ZipList ys
    
instance (Arbitrary a) => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary
