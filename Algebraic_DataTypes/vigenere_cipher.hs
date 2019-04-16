-- In the Lists chapter, you wrote a Caesar cipher. Now, we want to
-- expand on that idea by writing a Vigenère cipher. A Vigenère ci-
-- pher is another substitution cipher, based on a Caesar cipher, but it
-- uses a series of Caesar ciphers for polyalphabetic substitution. The
-- substitution for each letter in the plaintext is determined by a fixed
-- keyword.
-- 
-- So, for example, if you want to encode the message “meet at
-- dawn,” the first step is to pick a keyword that will determine which
-- Caesar cipher to use. We’ll use the keyword “ALLY” here. You repeat
-- the keyword for as many characters as there are in your original
-- message:
-- 
-- MEET AT DAWN
-- ALLY AL LYAL
-- 
-- Now the number of rightward shifts to make to encode each
-- character is set by the character of the keyword that lines up with it.
-- The ’A’ means a shift of 0, so the initial M will remain M. But the ’L’
-- for our second character sets a rightward shift of 11, so ’E’ becomes
-- ’P’. And so on, so “meet at dawn” encoded with the keyword “ALLY”
-- becomes “MPPR AE OYWY.”

-- Identifiers :
-- k - Key
-- p - Plain text
-- c - Single character

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
