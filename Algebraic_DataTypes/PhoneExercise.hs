module PhoneExercise where

import Data.List
import Data.Char

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Alphabet = Char
type Presses = Int

keypad :: [(Digit, [Alphabet])]
keypad = [   ('1', "1")
           , ('2', "abc2")
           , ('3', "def3")
           , ('4', "ghi4")
           , ('5', "jkl5")
           , ('6', "mno6")
           , ('7', "pqrs7")
           , ('8', "tuv8")
           , ('9', "wxyz9")
           , ('0', " 0")
           , ('*', [])
           , ('#', ".,")
        ]

mapAlphabetToButton :: Alphabet -> (Digit, [Alphabet])
mapAlphabetToButton a =
    case find (\(_, as) -> elem (toLower a) as) keypad of
        Just k -> k
        _ -> undefined

taps :: [Alphabet] -> Alphabet -> Int
taps xs x = case elemIndex (toLower x) xs of
                Just pos -> pos + 1
                _ -> 0

reverseTaps :: Alphabet -> [(Digit, Presses)]
reverseTaps x = case (isUpper x, mapAlphabetToButton x) of
                     (True,  (d, xs)) -> [('*', 1), (d, taps xs x)]
                     (False, (d, xs)) -> [          (d, taps xs x)]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd

frequency :: Ord a => [a] -> [(a, Int)]
frequency list = map (\k -> (head k, length k)) $ group $ sort list

mostFrequent :: Ord a => [a] -> a
mostFrequent = fst . maximumBy (\x y -> compare (snd x) (snd y)) . frequency

mostPopularLetter :: [Alphabet] -> Digit
mostPopularLetter = mostFrequent

cost :: [Alphabet] -> Presses
cost = sum . map (fingerTaps.reverseTaps)

flatten :: [[a]] -> [a]
flatten = foldr (++) []

coolestLtr :: Alphabet
coolestLtr = mostPopularLetter $ flatten convo

coolestWord :: [Alphabet]
coolestWord = mostFrequent $ words $ flatten $ intersperse " " convo
