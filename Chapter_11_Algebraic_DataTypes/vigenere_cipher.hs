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

module VigenereChipher where

import Data.Char

alphabetSize :: Int
alphabetSize = 26

fromInt :: Int -> Char
fromInt i = chr $ (i `mod` alphabetSize) + 65

toInt :: Char -> Int
toInt c = (ord c - 65) `mod` alphabetSize

encFunc :: Char -> Char -> Char
encFunc c k = fromInt $ (toInt c) + (toInt k)

decFunc :: Char -> Char -> Char
decFunc c k = fromInt $ (toInt c) - (toInt k)

crypt :: (Char -> Char -> Char) -> String -> String -> String
crypt _ [] _ = []
crypt f (' ':ps) ks = ' ' : crypt f ps ks
crypt f (p:ps) (k:ks) = (f p k) : crypt f ps ks

encrypt :: String -> String -> String
encrypt p k = crypt encFunc p (cycle k)

decrypt :: String -> String -> String
decrypt p k = crypt decFunc p (cycle k)
