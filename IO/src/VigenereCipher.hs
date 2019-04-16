{-# LANGUAGE PatternGuards #-}

module VigenereCipher where

import Data.Char

type Key = String
type PlainText = String
type Secret = String
type CryptFunction = Char -> Char -> Char


alphabetSize :: Int
alphabetSize = 26

fromInt :: Int -> Char
fromInt i = chr $ (i `mod` alphabetSize) + 65

toInt :: Char -> Int
toInt c = (ord c - 65) `mod` alphabetSize

add :: CryptFunction
add k c = fromInt $ (toInt c) + (toInt k)

sub :: CryptFunction
sub k c = fromInt $ (toInt c) - (toInt k)

apply :: CryptFunction -> Key -> String -> String
apply _ [] _ = []
apply _ _ [] = []
apply f all@(k:ks) (x:xs) =
    let go | isAlpha x = f k x : apply f ks xs
           | otherwise = x     : apply f all xs
    in go

encrypt :: Key -> PlainText -> Secret
encrypt k = apply add (cycle k)

decrypt :: Key -> Secret -> PlainText
decrypt k = apply sub (cycle k)
