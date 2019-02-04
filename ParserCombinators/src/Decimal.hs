module Decimal where
    
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

type DoubleOrRational = Either Double Rational

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
    a <- decimal
    char '.'
    b <- decimal
    return (read $ (show a) ++ "." ++ (show b) :: Double)

fractionOrDecimal :: Parser DoubleOrRational
fractionOrDecimal =
        (Left <$> try parseDecimal <?> "Decimal")
    <|> (Right <$> try parseFraction <?> "Fraction")

main :: IO ()
main = do
    print $ parseString fractionOrDecimal mempty "10.2"
    print $ parseString fractionOrDecimal mempty "10/2"
    print $ parseString fractionOrDecimal mempty "10"