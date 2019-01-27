module UnitOfSuccess where

import Text.Trifecta


integerEof :: Parser Integer
integerEof = do
    int <- integer
    eof
    pure int

main :: IO ()
main = do
    print $ parseString (integer >> eof) mempty "123"
    print $ parseString integerEof mempty "123"
    print $ parseString integerEof mempty "123abc"