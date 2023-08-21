{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.UMap (UMap)
import qualified Cardano.Ledger.UMap as UMap

import Control.Monad (replicateM)

import Criterion (Benchmark, bench, env, nf)
import Criterion.Main (defaultMain)

import qualified Data.Map.Strict as Map

import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.QuickCheck (arbitrary, generate)

main :: IO ()
main = do
  defaultMain $
    map (\c -> env (generateUMap c) umapSizeBench) (map (* 10000) [1 .. 2])
  where
    umapSizeBench :: UMap StandardCrypto -> Benchmark
    umapSizeBench umap =
      bench ("compositeSize (" ++ show (compositeSize umap) ++ ")") (nf compositeSize umap)

-- -------------------------------------------------------------------------------------------------

compositeSize :: UMap StandardCrypto -> Int
compositeSize umap =
  sum
    [ UMap.size (UMap.RewDepUView umap)
    , UMap.size (UMap.PtrUView umap)
    , UMap.size (UMap.SPoolUView umap)
    , UMap.size (UMap.DRepUView umap)
    ]

-- Generate a UView of exactly the size specified.
generateUMap :: Int -> IO (UMap StandardCrypto)
generateUMap size =
  generate $ do
    ptrs <- replicateM size arbitrary
    sPools <- replicateM size arbitrary
    dReps <- replicateM size arbitrary
    creds <- replicateM size arbitrary
    rdPairs <- replicateM size arbitrary
    pure $
      UMap.unify
        (Map.fromList $ zip creds rdPairs)
        (Map.fromList $ zip ptrs creds)
        (Map.fromList $ zip creds sPools)
        (Map.fromList $ zip creds dReps)
