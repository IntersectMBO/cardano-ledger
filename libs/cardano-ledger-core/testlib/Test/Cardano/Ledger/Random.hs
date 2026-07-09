{-# LANGUAGE BangPatterns #-}

module Test.Cardano.Ledger.Random (
  -- | Will need to find a better home in the future
  uniformSetElem,
  uniformMapElem,
  uniformSubSet,
  uniformSubMap,
  uniformSubMapElems,
  stdNormalDouble,
  logNormalDouble,
  truncatedLogNormalDouble,
  paretoDouble,
  truncatedParetoDouble,
  logUniformDouble,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random.Stateful (StatefulGen, uniformDouble01M, uniformDoublePositive01M, uniformRM)

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

uniformSetElem ::
  StatefulGen g m =>
  Set k ->
  g ->
  m (Maybe k)
uniformSetElem inputSet gen
  | numElems <= 0 = pure Nothing
  | otherwise = do
      ix <- uniformRM (0, numElems - 1) gen
      pure $ Just $ Set.elemAt ix inputSet
  where
    numElems = Set.size inputSet

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

uniformMapElem ::
  StatefulGen g m =>
  Map k v ->
  g ->
  m (Maybe (k, v))
uniformMapElem inputMap gen
  | numElems <= 0 = pure Nothing
  | otherwise = do
      ix <- uniformRM (0, numElems - 1) gen
      pure $ Just $ Map.elemAt ix inputMap
  where
    numElems = Map.size inputMap

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

-- * Probability distribution sampling functions, eg for use in generators

-- | Standard normal distribution, using <https://en.wikipedia.org/wiki/Box-Muller_transform>.
-- mu is 0, sigma is 1.
stdNormalDouble :: StatefulGen g m => g -> m Double
stdNormalDouble g = do
  u <- uniformDoublePositive01M g
  v <- uniformDouble01M g
  pure $ sqrt ((-2) * log u) * cos (2 * pi * v)

-- | See <https://en.wikipedia.org/wiki/Log-normal_distribution>.
-- Mean is `exp (mu + sigma^2 / 2)`.
-- Deviation is `mean * sqrt (exp (sigma^2) - 1)`.
logNormalDouble :: StatefulGen g m => Double -> Double -> g -> m Double
logNormalDouble mu sigma g = do
  z <- stdNormalDouble g
  pure $ exp (mu + sigma * z)

-- | See <https://en.wikipedia.org/wiki/Truncated_distribution>
truncatedLogNormalDouble :: StatefulGen g m => Double -> Double -> Double -> Double -> g -> m Double
truncatedLogNormalDouble mu sigma xMin xMax g = do
  x <- logNormalDouble mu sigma g
  if xMin <= x && x <= xMax
    then pure x
    else truncatedLogNormalDouble mu sigma xMin xMax g

-- | See <https://en.wikipedia.org/wiki/Pareto_distribution>.
-- Sampling uses the inverse CDF (Cumulative Distribution Function).
-- The distribution is naturally bounded at the lower end.
paretoDouble :: StatefulGen g m => Double -> Double -> g -> m Double
paretoDouble alpha xMin g = do
  u <- uniformDoublePositive01M g
  pure $ xMin / (u ** recip alpha)

-- | See <https://en.wikipedia.org/wiki/Truncated_distribution>
truncatedParetoDouble :: StatefulGen g m => Double -> Double -> Double -> g -> m Double
truncatedParetoDouble alpha xMin xMax g = do
  x <- paretoDouble alpha xMin g
  if x <= xMax
    then pure x
    else truncatedParetoDouble alpha xMin xMax g

-- | See <https://en.wikipedia.org/wiki/Reciprocal_distribution>.
-- Sampling uses the inverse CDF (Cumulative Distribution Function).
-- No truncation is necessary as the distribution is naturally bounded.
logUniformDouble :: StatefulGen g m => Double -> Double -> g -> m Double
logUniformDouble xMin xMax g = do
  u <- uniformDouble01M g
  pure $ xMin * (xMax / xMin) ** u
