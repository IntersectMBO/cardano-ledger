module Test.Utils
  ( assertAll
  ) where

import           Hedgehog (MonadTest, (===))

assertAll :: (MonadTest m, Show a, Eq a) => (a -> Bool) -> [a] -> m ()
assertAll p xs = [] === filter (not . p) xs
