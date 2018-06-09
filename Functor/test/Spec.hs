{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function
import Lib

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- Exercises: Instances of Func
-- Implement Functor instances for the following datatypes. Use the
-- QuickCheck properties we showed you to validate them.

-- Tests for Identity

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary        
        return $ Identity a        

type IdentityFC = Identity Int -> IntToInt -> IntToInt -> Bool

testIdentity = do
    quickCheck (functorIdentity :: (Identity Int) -> Bool)
    quickCheck (functorCompose' :: IdentityFC)

-- Tests for Pair 
instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

testPair = do
    quickCheck (functorIdentity :: (Pair Int) -> Bool)
    quickCheck (functorCompose' :: PairFC)



    -- Test for Two
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool

testTwo = do
    quickCheck (functorIdentity :: (Two Int Int) -> Bool)
    quickCheck (functorCompose' :: TwoFC)



-- Test for Three
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool

testThree = do
    quickCheck (functorIdentity :: (Three Int Int Int) -> Bool)
    quickCheck (functorCompose' :: ThreeFC)



-- Test for Three'
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'

type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool

testThree' = do
    quickCheck (functorIdentity :: (Three' Int Int) -> Bool)
    quickCheck (functorCompose' :: Three'FC)



main :: IO ()
main = do
    quickCheck (functorIdentity :: [Int] -> Bool)
    quickCheck (functorCompose' :: IntFC)
    testIdentity
    testPair
    testTwo
    testThree
    testThree'

