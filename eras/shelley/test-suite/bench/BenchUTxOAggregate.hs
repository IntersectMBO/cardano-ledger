{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BenchUTxOAggregate where

import Control.Iterate.SetAlgebra (compile, compute, run)
import Control.SetAlgebra (dom, (▷), (◁))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck

-- ====================================================================================
-- operations to benchmark different algorithms for executing ((dom d ◁ r) ▷ dom rg)

big :: Int -> Gen Int
big n = choose (1, n)

pair :: Gen a -> Gen b -> Gen (a, b)
pair g1 g2 = do x <- g1; y <- g2; pure (x, y)

char :: Gen Char
char = arbitrary

list :: Int -> Gen a -> Gen [a]
list m g = sequence [g | _x <- [1 .. m]]

expr :: Int -> IO (Map Int Int, Map Int Char, Map Char Char)
expr n = do d <- domain; r <- rel; rg <- range; pure (d, r, rg)
  where
    rel :: IO (Map Int Char)
    rel = fmap Map.fromList $ generate $ list n (pair (big 1000000) char)
    domain :: IO (Map Int Int)
    domain = fmap Map.fromList $ generate $ list (div n 7) (pair (big 1000000) (big 10))
    range :: IO (Map Char Char)
    range = fmap Map.fromList $ generate $ list (max 10 (div n 2000)) (pair char char)

test23 :: Map Int Int -> Map Int Char -> Map Char Char -> Bool
test23 domain relation range = compute exp2 == (run (compile exp2))
  where
    exp2 = ((dom domain ◁ relation) ▷ dom range)
