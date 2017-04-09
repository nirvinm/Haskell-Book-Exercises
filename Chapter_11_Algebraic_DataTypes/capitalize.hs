module Capitalize where 

import Data.Char
import Data.List ( intercalate )

-- Split a sentence into words, then tuple each word with the capi-
-- talized form of each.
capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map (\x -> (x, capitalizeWord x)) $ words xs

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x) : xs

-- Write a function that capitalizes sentences in a paragraph.
-- Recognize when a new sentence has begun by checking for periods.
-- Reuse the capitalizeWord function.
capitalizeParagraph :: String -> String
capitalizeParagraph p = intercalate ". " $ map (capitalizeWord.trim) $ split '.' p

trim :: String -> String
trim = unwords.words

split :: Char -> String -> [String]
split delimiter sentence =
    -- ws = processed words
    -- w  = current word
    let accumulate c (ws, w) = if c == delimiter then (w:ws, []) else (ws, c:w)
        reduce xs = foldr accumulate ([], []) xs
        result = reduce sentence
    in (snd result):(fst result)