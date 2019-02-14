module LogFile where

import Control.Applicative
import Data.Time
import Data.Map (Map)
import qualified Data.Map as M
import Text.Trifecta

-- 5. Write a parser for a log file format and sum the time spent in
-- each activity. Additionally, provide an alternative aggregation
-- of the data that provides average time spent per activity per day.
-- The format supports the use of comments which your parser
-- will have to ignore. The # characters followed by a date mark
-- the beginning of a particular day.

-- wheee a comment

-- # 2025-02-05
-- 08:00 Breakfast
-- 09:00 Sanitizing moisture collector
-- 11:00 Exercising in high-grav gym
-- 12:00 Lunch
-- 13:00 Programming
-- 17:00 Commuting home in rover
-- 17:30 R&R
-- 19:00 Dinner
-- 21:00 Shower
-- 21:15 Read
-- 22:00 Sleep

-- # 2025-02-07 -- dates not nececessarily sequential
-- 08:00 Breakfast -- should I try skippin bfast?
-- 09:00 Bumped head, passed out
-- 13:36 Wake up, headache
-- 13:37 Go to medbay
-- 13:40 Patch self up
-- 13:45 Commute home for rest
-- 14:15 Read
-- 21:00 Dinner
-- 21:15 Read
-- 22:00 Sleep

type Description = String
data Activity = Activity TimeOfDay Description deriving (Eq, Show)
data DayLog = DayLog Day [Activity] deriving (Eq, Show)

description :: Parser Description
description = some $ choice [alphaNum, char ' ']

eol :: Parser ()
eol = oneOf "\n" >> mempty

whitespace :: Parser ()
whitespace = char ' ' >> mempty

comment :: Parser ()
comment = string "-- " >> description >> mempty

junk :: Parser ()
junk = choice [comment, eol, whitespace]

skipJunk :: Parser ()
skipJunk = many junk >> mempty

date' :: Parser Day
date' = do
    _ <- string "# "
    y <- integer <?> "Year"
    _ <- char '-'
    m <- integer <?> "Month"
    _ <- char '-'
    d <- integer <?> "Day"
    skipJunk
    case fromGregorianValid y (fromInteger m) (fromInteger d) of
        Just d -> return d
        Nothing -> fail "Invalid date. Date should be in YYYY-MM-DD format."

time' :: Parser TimeOfDay
time' = do
   h <- integer <?> "Hour"
   _ <- char ':'
   m <- integer <?> "Time"
   return $ TimeOfDay (fromInteger h) (fromInteger m) 0

activity :: Parser Activity
activity =
    skipJunk *> (Activity <$> time' <*> description) <* skipJunk

dayLog :: Parser DayLog
dayLog =
    skipJunk *> (DayLog <$> date' <*> some activity) <* skipJunk

log' :: Parser [DayLog]
log' = some dayLog


durationPerActivity :: [Activity] -> Map Description DiffTime
durationPerActivity xs = 
    let toDiff t = timeOfDayToTime t
        go (Activity t1 desc)
           (Activity t2 _) = (desc, toDiff t2 - toDiff t1)
    in M.fromList $ zipWith go xs (tail xs)


avgTimePerActivity :: [DayLog] -> Map Description DiffTime
avgTimePerActivity logs =
    let avg x y = (x + y) / 2
    in M.unionsWith avg $ map (\(DayLog _ xs) -> durationPerActivity xs) logs
