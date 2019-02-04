module SemVer where

import Control.Applicative
import Text.Trifecta

-- 24.11 Chapter Exercises

-- 1. Write a parser for semantic versions as defined by http://semver.
-- org/. After making a working parser, write an Ord instance for the
-- SemVer type that obeys the specification outlined on the SemVer
-- website.

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.

-- Examples:
-- 0.0.5, 1.2.3, 2.1.1
-- 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7, 1.0.0-x.7.z.92,
-- 1.0.0-alpha+001, 1.0.0+20130313144700, 1.0.0-beta+exp.sha.5114f85
data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show)

instance Ord SemVer where
    (SemVer a b c _ _) `compare` (SemVer a' b' c' _ _) =
        (a `compare` a')
     <> (b `compare` b')
     <> (c `compare` c')


major :: Parser Major
major = integer <?> "Major version"

minor :: Parser Minor
minor = integer <?> "Minor version"

patch :: Parser Patch
patch = integer <?> "Patch number"

separator :: Parser Char
separator = char '.' <?> "Separator -> ."

numberOrString :: Parser NumberOrString
numberOrString =
      (NOSI <$> integer)
  <|> (NOSS <$> some letter)

valuesSeparatedByDot :: Parser [NumberOrString]
valuesSeparatedByDot = numberOrString `sepBy` (symbol ".")

release :: Parser [NumberOrString]
release = char '-' >> valuesSeparatedByDot

metadata :: Parser [NumberOrString]
metadata = char '+' >> valuesSeparatedByDot

parseSemVer :: Parser SemVer
parseSemVer = do
    maj <- major
    separator
    min <- minor
    separator
    pat <- patch
    rel <- option mempty SemVer.release
    met <- option mempty metadata
    return $  SemVer maj min pat rel met
