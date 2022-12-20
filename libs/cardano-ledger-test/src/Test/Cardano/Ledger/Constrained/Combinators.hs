{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Constrained.Combinators where

import Cardano.Ledger.Coin (Coin (..))
import Data.Char (chr)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck hiding (Fixed, total)

-- import Debug.Trace(trace)
-- import qualified Data.List as List
-- ==========================================================================

test1 :: Int -> (Int -> Gen Int -> Gen b) -> IO b
test1 n x = generate (x n (arbitrary :: Gen Int))

test2 :: Int -> (Int -> Gen Int -> Gen Char -> Gen b) -> IO b
test2 n x = generate (x n (arbitrary :: Gen Int) (elements [chr i | i <- [1 .. 128]]))

small :: Gen Int
small = elements [1 .. 6]

-- =======================================================================

setSized :: Ord a => Int -> Gen a -> Gen (Set a)
setSized size gen = do set <- (Set.fromList <$> vectorOf size gen); fixSet 25 size gen set

fixSet :: Ord a => Int -> Int -> Gen a -> Set a -> Gen (Set a)
fixSet numtrys size genA s = help numtrys s
  where
    help trys set
      | trys <= 0 =
          if Set.size set == size
            then pure set
            else error ("Ran out of trys in fixSet: " ++ show trys)
    help trys set = case compare (Set.size set) size of
      EQ -> pure set
      GT -> help (trys - 1) (Set.deleteMin set)
      LT -> do a <- genA; help (trys - 1) (Set.insert a set)

mapSized :: Ord a => Int -> Gen a -> Gen b -> Gen (Map a b)
mapSized size genA genB = setSized size genA >>= addRange
  where
    addRange set = Set.foldl' accum (pure Map.empty) set
    accum ansGen dom = do ans <- ansGen; rng <- genB; pure (Map.insert dom rng ans)

coinSized :: Int -> Gen Coin
coinSized n = pure (Coin (fromIntegral n))

setFromSubsetWithSize :: Ord a => Set a -> Int -> Gen a -> Gen (Set a)
setFromSubsetWithSize subset n gen = do
  additions <- setSized n gen
  pure (Set.union subset additions)

subsetFromSetWithSize :: (HasCallStack, Ord a) => Set a -> Int -> Gen (Set a)
subsetFromSetWithSize set n = help set Set.empty n
  where
    help source target count
      | Set.size source < count = error ("subsetFromSetWithSize: Can't make a subset of size " ++ show n ++ " from a smaller set of size " ++ show (Set.size set))
      | count <= 0 = pure target
      | otherwise = do
          item <- itemFromSet source
          help (Set.delete item set) (Set.insert item target) (count - 1)

mapFromSubset :: Ord a => Map a b -> Int -> Gen a -> Gen b -> Gen (Map a b)
mapFromSubset subset n genA genB = do
  additions <- mapSized n genA genB
  pure (Map.union subset additions)

{-  BROKEN 'n' could be too big for 'set'
subsetFromMap :: Map a b -> Int -> Gen (Map a b)
subsetFromMap set n |
subsetFromMap set n = do
  indexes <- vectorOf n (elements [0 .. Map.size set - 1])
  pure (List.foldl' (flip Map.deleteAt) set indexes)
-}

mapFromSet :: Ord a => Set a -> Gen b -> Gen (Map a b)
mapFromSet set genB = addRange set
  where
    addRange s = Set.foldl' accum (pure Map.empty) s
    accum ansGen dom = do ans <- ansGen; rng <- genB; pure (Map.insert dom rng ans)

itemFromSet :: HasCallStack => Set a -> Gen a
itemFromSet set | Set.null set = error ("itemFromSet : Can't take an item from the empty set.")
itemFromSet set = elements (Set.toList set)

mapFromRange :: Ord a => [b] -> Gen a -> Gen (Map a b)
mapFromRange bs genA = Map.fromList <$> mapM (\b -> do a <- genA; pure (a, b)) bs

mapFromDomRange :: Ord a => Set a -> [b] -> Map a b
mapFromDomRange dom bs = Map.fromList $ zip (Set.toList dom) bs

subsetSize :: Set a -> Gen Int
subsetSize s | Set.null s = pure 0
subsetSize s = frequency [(1, pure 0), (20, choose (1, n - 1)), (1, pure n)]
  where
    n = Set.size s

subsetFromSet :: (HasCallStack, Ord a) => Set a -> Gen (Set a)
subsetFromSet set = do
  n <- subsetSize set
  subsetFromSetWithSize set n
