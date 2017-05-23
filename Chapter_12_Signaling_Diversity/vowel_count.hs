-- Write a recursive function that takes a text/string, breaks it into
-- words, and counts the number of instances of ”the” followed by
-- a vowel-initial word.CHAPTER 12. SIGNALING ADVERSITY

-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
vowels :: [Char]
vowels = "aAeEiIoOuU"

isVowel :: Char -> Bool
isVowel = flip elem vowels

theFollowsVowels :: (String, String) -> Bool
theFollowsVowels (x, (y:_)) = case (x, isVowel y) of
                              ("the", True) -> True
                              ("The", True) -> True
                              _     -> False
theFollowsVowels _ = False

zipNextWord :: String -> [(String, String)]
zipNextWord s = zip (words s)  (tail $ words s)

countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = length [x | x <- map theFollowsVowels $ zipNextWord s, x == True]
