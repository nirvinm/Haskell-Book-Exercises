{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IniParser where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ
import Text.Trifecta

-- INI File Format

-- # comment
-- ; comment
-- [section]
-- host=wikipedia.org
-- alias=claw

newtype Header =
    Header String
    deriving (Eq, Ord, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

data Section =
    Section Header Assignments
    deriving (Eq, Show)

newtype Config =
    Config (Map Header Assignments)
    deriving (Eq, Show)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace =
    skipMany (char ' ' <|> char '\n')

skipComments :: Parser ()
skipComments =
    skipMany (do _ <- char ';' <|> char '#'
                 skipMany (noneOf "\n")
                 skipEOL)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
    char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = 
    parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) m = M.insert h a m


parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return $ Config mapOfSections



-----------------------------------------
-- Examples
-----------------------------------------

headerEx :: ByteString
headerEx = "[blah]"

commentEx :: ByteString
commentEx =
    "; last modified 1 April\
    \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
    "; blah\n; woot\n  \n;hah"

sectionEx :: ByteString
sectionEx =
    "; ignore me\n[states]\nChris=Texas"

assignmentEx :: ByteString
assignmentEx = "woot=1"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw
[whatisit]
red=intoothandclaw
|]
