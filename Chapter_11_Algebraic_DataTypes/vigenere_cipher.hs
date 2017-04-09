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


crypt' :: (Int -> Int -> Int) -> [Char] -> [Char] -> Int -> Int -> [Char] -> [Char]
-- crypt' applies given artithmetic function and does Vigenere encryption/decryption
-- Logic: 1. Take current character from the input
--        2. If current character is not space
--                1. Compute which character of key should be used for crypto
--                2. Apply crypto function and increment charCount
--                3. Set this as cipher character
--        3. If current character is space then set space itself as cipher character
--        4. Append the cipher character to the output string
--        5. Calculate cipher by passing continuations recursively.
-- input is the string to be encrypted
-- key is encyption/decryption password without space
-- iteration - Continuation passing - Current position in the input 
-- charCount - Continuation passing - Number of characters encountered in the input excluding spaces.
-- outputList - Continuation passing - Encrypted output string
crypt' cryptFunc input key iteration charCount outputList 
    | length input <= iteration = outputList
    | otherwise = 
        let c = input !! iteration
            chipherC = if c == ' ' then ' ' else shift cryptFunc c $ key !! (mod charCount $ length key)
            nextCharCount = if c == ' ' then charCount else charCount + 1 
        in crypt' cryptFunc input key (iteration+1) nextCharCount (outputList ++ [chipherC])


encrypt :: [Char] -> [Char] -> [Char]
encrypt i k = crypt' (+) i k 0 0 []

decrypt :: [Char] -> [Char] -> [Char]
decrypt i k = crypt' (-) i k 0 0 []

-- input = "MEET AT DAWN"
-- key = "ALLY"
-- output = "MPPR AE OYWY"