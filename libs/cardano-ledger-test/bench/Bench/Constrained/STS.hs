{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Constrained.STS where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Crypto
import Constrained
import Control.DeepSeq
import Criterion
import Test.Cardano.Ledger.Constrained.Conway

govEnv :: GovEnv (ConwayEra StandardCrypto)
govEnv = genFromSpecWithSeed 10 30 (govEnvSpec @ConwayFn)

singleProposalTreeSpec :: Specification ConwayFn ProposalTree
singleProposalTreeSpec = constrained $ \ppupTree ->
  [ wellFormedChildren ppupTree
  , allGASInTree ppupTree $ \gas ->
      isCon @"ParameterChange" (pProcGovAction_ . gasProposalProcedure_ $ gas)
  , forAll (snd_ ppupTree) (genHint $ (Just 2, 10))
  ]

stsBenchmarks :: Benchmark
stsBenchmarks =
  bgroup
    "constrainedSTS"
    [ benchSpec 10 30 "govEnvSpec" (govEnvSpec @ConwayFn)
    , benchSpec 13 30 "govProposalsSpec" govPropSpec
    , benchSpec 13 30 "singleProposalTreeSpec" singleProposalTreeSpec
    , bench "theProposalSpec" (nf (show . govProposalsSpec @ConwayFn) govEnv)
    ]
  where
    govPropSpec = govProposalsSpec @ConwayFn govEnv

benchSpec :: (HasSpec fn a, NFData a) => Int -> Int -> String -> Specification fn a -> Benchmark
benchSpec seed size nm spec =
  bench (unlines [nm, show (genFromSpecWithSeed seed size spec)]) $
    nf (genFromSpecWithSeed seed size) spec
