{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Constrained.Combinators where

import Cardano.Ledger.Coin (Coin (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck hiding (Fixed, total)

-- ==========================================================================
-- Tracking Gen-time errors

-- | Report a Gen-time error from the current message 'extra' and the
--   [messages] 'mess' describing the path to this call site.
errorMess :: HasCallStack => String -> [String] -> a
errorMess extra mess = error (unlines ("\nGen-time error" : reverse (extra : mess)))

-- | suchThat version that tracks Gen-time errors
suchThatErr :: [String] -> Gen a -> (a -> Bool) -> Gen a
suchThatErr msgs gen p = sized $ \n -> try (10 :: Int) n
  where
    try 0 _ = errorMess "SuchThat times out" msgs
    try k sz = do
      x <- resize sz $ suchThatMaybe gen p
      case x of
        Just y -> pure y
        Nothing -> try (k - 1) (sz + 5)

-- =======================================================================

-- | add items from 'source' to 'base' until size 'n' is reached.
addUntilSize :: Ord a => [String] -> Set a -> Set a -> Int -> Gen (Set a)
addUntilSize msgs base source n = do
  let possible = Set.difference source base
      p = Set.size possible
      m = Set.size base
      loop result _ | Set.size result >= n = pure result
      loop _ extra
        | Set.null extra =
            errorMess
              ( "There are not enough unused elements in 'source'("
                  ++ show p
                  ++ ") to reach the size 'n'("
                  ++ show n
                  ++ ")"
              )
              msgs
      loop result extra = do
        i <- choose (0, Set.size extra - 1)
        loop (Set.insert (Set.elemAt i extra) result) (Set.deleteAt i extra)
  case compare m n of
    EQ -> pure base
    GT -> errorMess ("The size(" ++ show m ++ ") of the 'base' set exceeds the target size(" ++ show n ++ ")") msgs
    LT -> loop base possible

-- | Generate a random set of a fixed size 'size', use 'gen' to pick the elements.
setSized :: Ord a => [String] -> Int -> Gen a -> Gen (Set a)
setSized mess size gen = do
  set <- Set.fromList <$> vectorOf size gen
  fixSet (("setSized " ++ show size) : mess) 1000 size gen set

-- | Fix a a set 'source' that is supposed to have 'size' elements, but because of duplicates
--   may not. Fix it by adding random elements using 'genA', not currently in the set. If after 'numTries'
--   the set is still not fixed, report an error.
fixSet :: Ord a => [String] -> Int -> Int -> Gen a -> Set a -> Gen (Set a)
fixSet mess numTries size genA source
  | Set.size source > size = subsetFromSetWithSize ("fixSet" : mess) source size
  | otherwise = go numTries source genA
  where
    go n !set gen
      | currentSize == size = pure set
      | n < 0 =
          errorMess
            ( "Ran out of tries("
                ++ show numTries
                ++ ") in fixSet: need "
                ++ show size
                ++ " elements, have "
                ++ show currentSize
            )
            mess
      | otherwise = do
          x <- gen
          if x `Set.member` set
            then go (n - 1) set $ scale (+ 5) gen
            else go n (Set.insert x set) gen
      where
        currentSize = Set.size set

-- | Generate a random Map with a fixed size 'n'. use 'genA' to pick the
--   domain of the map, and 'genB' to pick the range.
mapSized :: Ord a => [String] -> Int -> Gen a -> Gen b -> Gen (Map a b)
mapSized mess size genA genB = do
  keys <- setSized (("mapSized " ++ show size) : mess) size genA
  values <- vectorOf size genB
  pure $ mapFromDomRange keys values

coinSized :: Int -> Gen Coin
coinSized n = pure (Coin (fromIntegral n))

-- | Generate a random subset of a set 'set' with a fixed size 'n'
subsetFromSetWithSize :: Ord a => [String] -> Set a -> Int -> Gen (Set a)
subsetFromSetWithSize mess set n
  | Set.size set < n =
      errorMess
        ( "subsetFromSetWithSize: Can't make a subset of size "
            ++ show n
            ++ " from a smaller set of size "
            ++ show (Set.size set)
        )
        mess
  -- It is faster to remove extra elements, when we need to drop more than a half
  | Set.size set `div` 2 < n = fst <$> help (flip const) set Set.empty (Set.size set - n)
  -- It is faster to pick out random elements when we only need under a half of the original set.
  | otherwise = snd <$> help seq set Set.empty n
  where
    help optSeq !source target count
      | count <= 0 = pure (source, target)
      | otherwise = do
          (item, source') <- itemFromSet (("subsetFromSetWithSize " ++ show n) : mess) source
          -- To avoid a memory leak we only force the target when it is the set we want to keep.
          let target' = Set.insert item target
          target' `optSeq` help optSeq source' target' (count - 1)

-- | Generate a larger map, from a smaller map 'subset'. The new larger map, should have all the
--   keys and values of the smaller map.
mapFromSubset :: Ord a => [String] -> Map a b -> Int -> Gen a -> Gen b -> Gen (Map a b)
mapFromSubset mess subset n genA genB = do
  additions <- mapSized (("mapFromSubset " ++ show n) : mess) n genA genB
  pure (Map.union subset additions)

-- | Generate a map, from a set. The new map, should have all the
--   elements of 'set' in its keysSet.
mapFromSet :: Ord a => Set a -> Gen b -> Gen (Map a b)
mapFromSet set genB = addRange set
  where
    addRange s = Set.foldl' accum (pure Map.empty) s
    accum ansGen dom = do ans <- ansGen; rng <- genB; pure (Map.insert dom rng ans)

-- | Generate a random element from a set. Also return the set with that element removed
itemFromSet :: [String] -> Set a -> Gen (a, Set a)
itemFromSet mess set
  | Set.null set = errorMess "itemFromSet : Can't take an item from the empty set." mess
itemFromSet _ set =
  (\ix -> (Set.elemAt ix set, Set.deleteAt ix set)) <$> chooseInt (0, Set.size set - 1)

-- Generate a map from a list 'bs'. The result should have the property (Map.elems result == bs)
mapFromRange :: forall a b. Ord a => [String] -> [b] -> Gen a -> Gen (Map a b)
mapFromRange msgs bs genA = do
  keys <- setSized msgs (length bs) genA
  pure $ mapFromDomRange keys bs

mapFromProj :: Ord a => [b] -> Gen a -> (b -> Gen c) -> Gen (Map a c)
mapFromProj bs genA genC = do
  cs <- mapM genC bs
  mapFromRange ["mapFromProj"] cs genA

-- | Generate a Map from a set 'dom' and a list 'bs'. They should have the same size
--   but this is not checked. The result should have the properties (Map.keysSet result == dom)
--   and (Map.elems result == bs)
mapFromDomRange :: Ord a => Set a -> [b] -> Map a b
mapFromDomRange dom bs = Map.fromAscList $ zip (Set.toAscList dom) bs

-- | Generate an Int, that would be a valid size for a subset of a set 's'
--   it should return 0 or the size of the set infrequently.
subsetSize :: Set a -> Gen Int
subsetSize s | Set.null s = pure 0
subsetSize s = frequency [(1, pure 0), (20, choose (1, n - 1)), (1, pure n)]
  where
    n = Set.size s

-- | Generate a random subset of a set 'set'
subsetFromSet :: Ord a => [String] -> Set a -> Gen (Set a)
subsetFromSet mess set = do
  n <- subsetSize set
  subsetFromSetWithSize ("From subsetFromSet" : mess) set n

-- | Generate a random superset of a set 'set'. Use 'genA' to pick random additional elements
superSetFromSet :: Ord a => Gen a -> Set a -> Gen (Set a)
superSetFromSet genA setA = do
  n <- choose (0, 4)
  if n == 0
    then pure setA
    else superSetFromSetWithSize ["supersetFromSet " ++ show n] (n + Set.size setA) genA setA

-- | Generate a random superset of a set 'set' with s fixed size 'n'.
--   Use 'genA' to pick random additional elements.
superSetFromSetWithSize :: Ord a => [String] -> Int -> Gen a -> Set a -> Gen (Set a)
superSetFromSetWithSize mess n _ setA
  | n < Set.size setA =
      errorMess
        ( "superSetFromSetWithSize: Size of the superset ("
            ++ show n
            ++ ") is smaller than "
            ++ "the size of the given set ("
            ++ show (Set.size setA)
            ++ ")."
        )
        mess
superSetFromSetWithSize msgs size genA setA =
  let tries = 10 * (size - Set.size setA) -- at most 10 times per new element
   in fixSet ("superSetFromSetWithSize" : msgs) tries size genA setA

-- | Generates things both in (Set a) and not in (Set a) with differing frequencies.
superItemFromSet :: Ord a => Gen a -> Set a -> Gen a
superItemFromSet genA set | Set.null set = genA
superItemFromSet genA set =
  frequency
    [ (3, fst <$> itemFromSet ["Not possible since set is not empty"] set)
    , (1, suchThat genA (`Set.notMember` set))
    ]

-- | Pick a random (key,value) pair from a Map
genFromMap :: [String] -> Map k a -> Gen (k, a)
genFromMap msgs m
  | n == 0 = errorMess "The map is empty in genFromMap" msgs
  | otherwise = do
      i <- choose (0, n - 1)
      pure $ Map.elemAt i m
  where
    n = Map.size m
