module Queue where

-- From Okasaki's Purely
-- Functional Data Structures
-- Description: Queue needs to do push and pop in O(1) access time.
--              List supports cheap prepending but not cheap appending. So we
--              model Queue not based on single List but two lists called
--              Inbox and Outbox. Push prepends item to Inbox which is cheap.
--              Pop checks if Outbox is empty. If it is, then it moves data from
--              Inbox to Outbox in reverse. Then returns the head. 
data Queue a =
    Queue { enqueue :: [a] -- inbox
          , dequeue :: [a] -- outbox
          } deriving (Eq, Show)
    
-- create an empty queue
empty :: Queue a
empty = Queue [] []

-- create a queue from list
fromList :: [a] -> Queue a
fromList ls = Queue [] ls

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue enq deq) = Queue (x:enq) deq

-- return and delete the oldest item
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue enq []) = pop $ Queue [] (reverse enq)
pop (Queue enq (x:xs)) = Just (x, Queue enq xs)
