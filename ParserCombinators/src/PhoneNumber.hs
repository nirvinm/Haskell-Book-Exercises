module PhoneNumber where

import Control.Applicative
import Text.Trifecta

-- 4. Write a parser for US/Canada phone numbers with varying
-- formats.
-- Examples:
-- "123-456-7890"
-- "1234567890"
-- "(123) 456-7890"
-- "1-123-456-7890"

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

betweenBraces :: Parser a -> Parser a
betweenBraces p = char '(' *> p <* char ')'

areaCode :: Parser NumberingPlanArea
areaCode = read <$> count 3 digit

prefix :: Parser String
prefix = string "1-"

parseAreaCode :: Parser NumberingPlanArea
parseAreaCode = token $
      skipOptional prefix
  >>  betweenBraces areaCode <|> areaCode

parseExchange :: Parser Exchange
parseExchange = token $ read <$> count 3 digit

parseLineNumber :: Parser LineNumber
parseLineNumber = read <$> count 4 digit

parsePhone :: Parser PhoneNumber
parsePhone = do
    areaCode <- parseAreaCode
    skipOptional (char '-')
    exchange <- parseExchange
    skipOptional (char '-')
    lineNumber <- parseLineNumber
    return $ PhoneNumber areaCode exchange lineNumber