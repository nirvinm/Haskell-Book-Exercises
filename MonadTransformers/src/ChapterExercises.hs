module ChapterExercises where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 1. rDec is a function that should get its argument in the context of
-- Reader and return a value decremented by one.
-- 2. Once you have an rDec that works, make it and any inner lambdas
-- pointfree if that’s not already the case.
rDec :: Num a => Reader a a
--rDec = reader $ \r -> r - 1
rDec = reader $ (-) 1


-- 3. rShow is show, but in Reader.
-- 4. Once you have an rShow that works, make it pointfree.
rShow :: Show a => ReaderT a Identity String
--rShow = ReaderT $ \r -> Identity (show r)
rShow = ReaderT $ Identity . show


-- 5. rPrintAndInc will first print the input with a greeting, then return
-- the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
    r <- ask
    -- _ <- return $ putStrLn (show r)
    lift $ putStrLn (show r)
    return $ r + 1


-- 6. sPrintIncAccum first prints the input with a greeting, then puts
-- the incremented input as the new state, and returns the original
-- input as a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
    s <- get
    lift $ putStrLn (show s)
    put $ s+1
    return (show s)
