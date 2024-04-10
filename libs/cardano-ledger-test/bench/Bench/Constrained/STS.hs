{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Constrained.STS where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Crypto
import Constrained
import Constrained.Bench
import Criterion
import Test.Cardano.Ledger.Constrained.Conway

govEnv :: GovEnv (ConwayEra StandardCrypto)
govEnv = genFromSpecWithSeed 10 30 (govEnvSpec @ConwayFn)

singleProposalTreeSpec :: Spec ConwayFn ProposalTree
singleProposalTreeSpec = constrained $ \ppupTree ->
  [ wellFormedChildren (lit SNothing) ppupTree
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
