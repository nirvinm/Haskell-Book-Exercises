module SimpleArithmeticTest where

import Lib
import Data.List (sort)

-- 1. for a function
halfIdentity = (*2).half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity n = (halfIdentity n) == n

-- 2. For any list you apply sort to
-- this property should hold
prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t)  = (Just y, x >= y)

prop_stringListOrdered :: [String] -> Bool
prop_stringListOrdered ls = prop_listOrdered (sort ls)

-- 3. Now we’ll test the associative and commutative properties
-- of addition:
plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
    x + y == y + x

-- 4. Now do the same for multiplication.
productAssociative :: Int -> Int -> Int -> Bool
productAssociative x y z =
    x * (y * z) == (x * y) * z

productCommutative :: Int -> Int -> Bool
productCommutative x y =
    x * y == y * x

-- 5. We mentioned in one of the first chapters that there are
-- some laws involving the relationship of quot and rem and
-- div and mod. Write QuickCheck tests to prove them.
-- quot rem
prop_quotRem :: Int -> Int -> Bool
prop_quotRem 0 _ = True
prop_quotRem _ 0 = True
prop_quotRem x y = (quot x y)*y + (rem x y) == x

prop_divMod :: Int -> Int -> Bool
prop_divMod 0 _ = True
prop_divMod _ 0 = True
prop_divMod x y = (div x y)*y + (mod x y) == x

-- 6. Is (^) associative? Is it commutative? Use QuickCheck to see
-- if the computer can contradict such an assertion.
prop_powerNotAssociative :: Int -> Int -> Int -> Bool
prop_powerNotAssociative x y z = x ^ (y ^ z) /= (x ^ y) ^ z

prop_powerNotCommutative :: Int -> Int -> Bool
prop_powerNotCommutative x y = x ^ y /= y ^ x

-- 7. Test that reversing a list twice is the same as the identity
-- of the list:
prop_listDoubleReverse :: [String] -> Bool
prop_listDoubleReverse ls = (reverse $ reverse ls) == id ls

-- 8. Write a property for the definition of ($).
prop_dollarFunction :: Int -> Bool
prop_dollarFunction x = (id $ x) == (id x)

prop_compositionFunction :: Int -> Bool
prop_compositionFunction x = (id.id $ x) == (id $ id x)

-- 9. See if these two functions are equal:
-- foldr (++) [] == concat
prop_cons :: Int -> Int -> Bool
prop_cons x y = foldr (:) [] [x, y] == [x] ++ [y]

prop_concat :: Int -> Int -> Bool
prop_concat x y  = foldr (++) [] [[x], [y]] == concat [[x], [y]]

-- 10. Hm. Is that so?
f :: Int -> [String] -> Bool
f n xs = length (take n xs) == n

-- 11. Finally, this is a fun one. You may remember we had you
-- compose read and show one time to complete a “round
-- trip.” Well, now you can test that it works:
f' :: Char -> Bool
f' x = (read (show x)) == x