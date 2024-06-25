{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

import Constrained
import Constrained.Base
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck hiding (forAll)

mapElemSpec :: Specification BaseFn (Map Int (Bool, Int))
mapElemSpec = constrained $ \m ->
  [ assert $ m /=. lit mempty
  , forAll' (rng_ m) $ \_ b ->
      [0 <. b, b <. 10]
  ]

mapSet :: Specification BaseFn (Map (Set Int) Int)
mapSet = constrained $ \x ->
  forAll (dom_ x) $ \d -> assert (subset_ d (lit (Set.fromList [3 .. 4])))

mapSet2 :: Specification BaseFn (Map (Set Int) Int)
mapSet2 = constrained $ \x ->
  forAll x $ \d ->
    match d $ \k v -> assert (subset_ k (lit (Set.fromList [3 .. 4])))

smallSet :: Specification BaseFn (Set Int)
smallSet = constrained $ \x ->
  forAll x $ \y -> elem_ y (lit [3])
