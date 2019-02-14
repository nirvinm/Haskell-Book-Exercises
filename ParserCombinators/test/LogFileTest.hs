{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFileTest where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Text.RawString.QQ
import Text.Trifecta
import Test.Hspec

import LogFile

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

sampleLog :: String
sampleLog = [r|
# 2019-2-9 -- Great day
10:30 Breakfast
-- not so great movie
11:00 Movie -- but you can watch this once
13:30 Sports
04:00 Sleep

-- this was a long day
# 2019-2-8 -- the day before
10:00 Breakfast
11:00 Sports
12:00 Movie -- spaces are considered into description
14:45 Sleep
-- end
|]

logParserTest :: IO ()
logParserTest = hspec $ do

    describe "Junk Parsing" $
        it "Can parse comments, newline, spaces" $ do
            let m = parseString skipJunk mempty "-- hot day\n-- cool day\n\n-- wow"
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ()

    describe "Date Parsing" $
        it "Can parse Date" $ do
            let m = parseString date' mempty "# 2019-02-09  -- a few comments wont hurt"
                r' = maybeSuccess m
            print m
            r' `shouldBe` (fromGregorianValid 2019 02 09)

    describe "Activity Parsing" $
        it "Can parse Time and Description" $ do
            let m = parseString activity mempty "12:30 Had Breakfast"
            print m

    describe "DayLog Parsing" $
        it "Can parse log for single day" $ do
            let m = parseString dayLog mempty sampleLog
            print m

    describe "Log Parsing" $
        it "Can parse log for multiple days" $ do
            let m = parseString log' mempty sampleLog
            print m

    describe "Average Time Calculation" $
        it "Calculates average time spent in each activity" $ do
            let m = avgTimePerActivity <$> parseString log' mempty sampleLog
            print m
