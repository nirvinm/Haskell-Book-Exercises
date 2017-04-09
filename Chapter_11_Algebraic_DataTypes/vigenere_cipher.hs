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
-- 
-- Like the Caesar cipher, you can find all kinds of resources to
-- help you understand the cipher and also many examples written
-- in Haskell. Consider using a combination of chr, ord, and mod again,
-- possibly very similar to what you used for writing the original Caesar
-- cipher.

module VigenereChipher where

import Data.Char

shift :: (Int -> Int -> Int) -> Char -> Char -> Char
-- shift applies a arithmetic function to given characters
-- after converting it from ASCII value to Int
-- This can be used to Encrypt or Decrypt the character
shift f i k = let i' = ord i - 65
                  k' = ord k - 65
              in  chr $ ((f i' k') `mod` 26) + 65


crypt :: (Int -> Int -> Int) -> (String, Int) -> (String, Int) -> String -> String
-- crypt applies given artithmetic function and does Vigenere encryption/decryption.
-- This function moves forward on the string, encrypts the
-- current char using corresponding char in key. It maintains
-- current position of the string and key it is working on. When
-- it encounters space, it skips the en(de)cryption and doen't move forward
-- in the key. After encrypting the current char, it recursively calls
-- itself by passing next positions in string to be encrypted along with
-- position of key.
crypt cryptFunc (words', wpos) (key, kpos) cipherText
    | length words' <= wpos =  cipherText
    | otherwise =
        let c  = words' !! wpos
            c' = case c of 
                    ' ' -> ' '
                    _ -> shift cryptFunc c $  key !! kpos
            kpos' = case c of
                    ' ' -> kpos `mod` (length key)
                    _   -> (kpos + 1) `mod` (length key)
            in crypt cryptFunc (words', wpos + 1) (key, kpos') (cipherText ++ [c'])

encrypt :: [Char] -> [Char] -> [Char]
encrypt i k = crypt (+) (i, 0) (k, 0) []

decrypt :: [Char] -> [Char] -> [Char]
decrypt i k = crypt (-) (i, 0) (k, 0) []

-- input = "MEET AT DAWN"
-- key = "ALLY"
-- output = "MPPR AE OYWY"
