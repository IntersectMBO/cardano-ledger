-- | Provides a First In First Out queue with amortised constant-time operations
module Data.Queue
  ( Queue
  , newQueue
  , pushQueue
  , popQueue
  , headQueue
  , isQueueEmpty
  , sizeQueue
  )
where

data Queue a = MkQueue { inbox :: [a], outbox :: [a] }

instance Show a => Show (Queue a) where
  show (MkQueue i o) = (show $ o ++ reverse i)


-- | Creates a new empty 'Queue'
newQueue :: Queue a
newQueue = MkQueue [] []

-- | Pushes a value to the 'Queue'
pushQueue :: a -> Queue a -> Queue a
pushQueue e (MkQueue inb out) = MkQueue (e : inb) out

-- | Attempts to pop a value from the queue
popQueue :: Queue a -> Maybe (Queue a)
popQueue (MkQueue [] []      ) = Nothing
popQueue (MkQueue i  []      ) = popQueue (MkQueue [] (reverse i))
popQueue (MkQueue i  (_ : os)) = Just (MkQueue i os)

-- | Attempts to retrieve the head of the queue
headQueue :: Queue a -> Maybe (a, Queue a)
headQueue (  MkQueue [] []     ) = Nothing
headQueue (  MkQueue i  []     ) = headQueue (MkQueue [] (reverse i))
headQueue q@(MkQueue _  (o : _)) = Just (o, q)

-- | Checks if the queue is empty
isQueueEmpty :: Queue a -> Bool
isQueueEmpty (MkQueue [] []) = True
isQueueEmpty _               = False

sizeQueue :: Queue a -> Word
sizeQueue (MkQueue inb oub) = fromIntegral $ length inb + length oub
