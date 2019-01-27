module ParsingPractice where

import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "Stop"

one :: Parser Char
one = char '1'

one' ::  Parser b
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

-- Exercises: Parsing Practice

-- 1. There’s a combinator that’ll let us mark that we expect an input
-- stream to be finished at a particular point in our parser. In the
-- parsers library this is simply called eof (end-of-file) and is in the
-- Text.Parser.Combinators module. See if you can make the one and
-- oneTwo parsers fail because they didn’t exhaust the input stream!
one_ :: Parser ()
one_ = one >> eof

oneTwo_ :: Parser ()
oneTwo_ = oneTwo >> eof

-- 2. Use string to make a Parser that parses “1”, “12”, and “123” out of
-- the example input respectively. Try combining it with stop too.
-- That is, a single parser should be able to parse all three of those
-- strings. An example:
-- Prelude> p123 "1"
-- Success 1
-- Prelude> p123 "12"
-- Success 12
-- Prelude> p123 "123"
-- Success 123
p123 :: Parser String
p123 = choice [string "123", string "12", string "1"]


-- 3. Try writing a Parser that does what string does, but using char.
string' :: String -> Parser String
string' s = go s mempty
    where
        go (x:xs) parsed = char x >>= (\x' -> go xs (parsed ++ [x']))
        go [] parsed = pure parsed


pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    pNL "p123:"
    print $ parseString p123 mempty "123"
    pNL "string':"
    print $ parseString (string' "Hello") mempty "Hello"