module Integer where

import Control.Applicative
import Text.Trifecta

-- 2. Write a parser for positive integer values. Don’t reuse the pre-
-- existing digit or integer functions, but you can use the rest of
-- the libraries we’ve shown you so far. You are not expected to
-- write a parsing library from scratch.

-- More efficient than using 'Ord' + toInteger
digitToInt :: Char -> Integer
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt _ = 0

stringToInt :: [Char] -> Integer
stringToInt ls = go 0 ls
    where go acc [] = acc
          go acc (x:xs) = go ((acc * 10) + (digitToInt x)) xs

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Integer :: Parser Integer
base10Integer = stringToInt <$> some parseDigit <?> "Integer"

-- 3. Extend the parser you wrote to handle negative and positive
-- integers. Try writing a new parser in terms of the one you
-- already have to do this.
negativeInteger :: Parser Integer
negativeInteger = char '-' >> (negate <$> base10Integer) <?> "Negative Integer"

positiveInteger :: Parser Integer
positiveInteger = base10Integer <?> "Positive Integer"

base10Integer' :: Parser Integer
base10Integer' = negativeInteger <|> positiveInteger
