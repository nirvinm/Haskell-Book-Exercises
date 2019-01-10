module ChapterExercises where

-- Write State for yourself
newtype State s a = State { runState :: s -> (a, s) }

--1. Construct a State where the state is also the value you return.
get :: State s s
get = State $ \s -> (s, s)

-- Expected output
-- Prelude> runState get "curryIsAmaze"
-- ("curryIsAmaze","curryIsAmaze")

--2. Construct a State where the resulting state is the argument
---  provided and the value is defaulted to unit.
put :: s -> State s ()
put s = State $ \s0 -> ((), s)

-- Prelude> runState (put "blah") "woot"
-- ((),"blah")

-- 3. Run the State with s and get the state that results.
exec :: State s a -> s -> s
exec (State sa)= snd.sa

-- Prelude> exec (put "wilma") "daphne"
-- "wilma"
-- Prelude> exec get "scooby papu"
-- "scooby papu"

-- 4. Run the State with s and get the value that results.
eval :: State s a -> s -> a
eval (State sa) = fst.sa

-- Prelude> eval get "bunnicula"
-- "bunnicula"
-- Prelude> eval get "stake a bunny"
-- "stake a bunny"

-- 5. Write a function which applies a function to create a new State.
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Should behave like the following:
-- Prelude> runState (modify (+1)) 0
-- ((),1)
-- Prelude> runState (modify (+1) >> modify (+1)) 0
-- ((),2)