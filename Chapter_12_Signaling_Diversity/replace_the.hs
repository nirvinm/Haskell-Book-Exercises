-- 1. Write a recursive function named replaceThe which takes a text/string,
-- breaks it into words and replaces each instance of “the” with “a”.
-- It’s intended only to replace exactly the word “the”. notThe is a
-- suggested helper function for accomplishing this.

-- example GHCi session above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe "The" = Nothing
notThe w     = Just w

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe s = unwords $ map (replace.notThe) (words s)
                            where replace Nothing  = "a"
                                  replace (Just w) = w

