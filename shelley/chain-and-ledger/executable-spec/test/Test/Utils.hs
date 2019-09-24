module Test.Utils
  ( all'
  ) where

import Hedgehog ((===), MonadTest)

all' :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
all' p xs = [] === filter (not . p) xs
