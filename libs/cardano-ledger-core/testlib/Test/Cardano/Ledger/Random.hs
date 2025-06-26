{-# LANGUAGE BangPatterns #-}

module Test.Cardano.Ledger.Random (
  -- | Will need to find a better home in the future
  uniformSubSet,
  uniformSubMap,
  uniformSubMapElems,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random.Stateful (StatefulGen, uniformRM)

-- | Either clamp requested size to the range of @[0, actualSize]@ or generate at random
-- in the same range when requested size is not supplied.
uniformSubSize ::
  StatefulGen g m =>
  -- | Requested size
  Maybe Int ->
  -- | Actual size
  Int ->
  g ->
  m Int
uniformSubSize mReqSize actualSize gen =
  case mReqSize of
    Nothing -> uniformRM (0, actualSize) gen
    Just reqSize -> pure $ max 0 $ min actualSize reqSize

uniformSubSet ::
  (StatefulGen g m, Ord k) =>
  -- | Size of the subset. If supplied will be clamped to @[0, Set.size s]@ interval,
  -- otherwise will be generated randomly.
  Maybe Int ->
  Set k ->
  g ->
  m (Set k)
uniformSubSet mSubSetSize inputSet gen = do
  subSetSize <- uniformSubSize mSubSetSize (Set.size inputSet) gen
  if subSetSize < Set.size inputSet `div` 2
    then
      goAdd inputSet Set.empty subSetSize
    else
      goDelete inputSet (Set.size inputSet - subSetSize)
  where
    -- Constructing a new Set is faster when less then a half of original Set will be used
    goAdd !s !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Set.size s - 1) gen
          goAdd (Set.deleteAt ix s) (Set.insert (Set.elemAt ix s) acc) (i - 1)
    -- Deleting is faster when more items need to be retained in the Set
    goDelete !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Set.size acc - 1) gen
          goDelete (Set.deleteAt ix acc) (i - 1)

uniformSubMap ::
  (StatefulGen g m, Ord k) =>
  -- | Size of the subMap. If supplied will be clamped to @[0, Map.size s]@ interval,
  -- otherwise will be generated randomly.
  Maybe Int ->
  Map k v ->
  g ->
  m (Map k v)
uniformSubMap mSubMapSize inputMap gen = do
  subMapSize <- uniformSubSize mSubMapSize (Map.size inputMap) gen
  if subMapSize < Map.size inputMap `div` 2
    then
      -- Constructing a new Map is faster when less then a half of original Map will be used
      uniformSubMapElems Map.insert (Just subMapSize) inputMap gen
    else
      -- Deleting is faster when more items need to be retained in the Map
      goDelete inputMap (Map.size inputMap - subMapSize)
  where
    goDelete !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Map.size acc - 1) gen
          goDelete (Map.deleteAt ix acc) (i - 1)

uniformSubMapElems ::
  (StatefulGen g m, Monoid f) =>
  (k -> v -> f -> f) ->
  -- | Size of the subMap. If supplied will be clamped to @[0, Map.size s]@ interval,
  -- otherwise will be generated randomly.
  Maybe Int ->
  Map k v ->
  g ->
  m f
uniformSubMapElems insert mSubMapSize inputMap gen = do
  subMapSize <- uniformSubSize mSubMapSize (Map.size inputMap) gen
  go inputMap mempty subMapSize
  where
    go !s !acc !i
      | i <= 0 = pure acc
      | otherwise = do
          ix <- uniformRM (0, Map.size s - 1) gen
          let (k, v) = Map.elemAt ix s
          go (Map.deleteAt ix s) (insert k v acc) (i - 1)
