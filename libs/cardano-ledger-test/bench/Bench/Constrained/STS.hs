{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Constrained.STS where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Rules
import Constrained.API
import Control.DeepSeq
import Criterion
import Test.Cardano.Ledger.Constrained.Conway

govEnv :: GovEnv ConwayEra
govEnv = genFromSpecWithSeed 10 30 govEnvSpec

singleProposalTreeSpec :: Specification (ProposalTree ConwayEra)
singleProposalTreeSpec = constrained $ \ppupTree ->
  [ wellFormedChildren ppupTree
  , satisfies
      ppupTree
      ( allGASInTree
          ( \gas ->
              isCon @"ParameterChange" (pProcGovAction_ . gasProposalProcedure_ $ gas)
          )
      )
  , forAll (snd_ ppupTree) (genHint $ (Just 2, 10))
  ]

stsBenchmarks :: Benchmark
stsBenchmarks =
  bgroup
    "constrainedSTS"
    [ benchSpec 10 30 "govEnvSpec" govEnvSpec
    , benchSpec 13 30 "govProposalsSpec" govPropSpec
    , benchSpec 13 30 "singleProposalTreeSpec" singleProposalTreeSpec
    , bench "theProposalSpec" (nf (show . govProposalsSpec) govEnv)
    ]
  where
    govPropSpec = govProposalsSpec govEnv

benchSpec :: (HasSpec a, NFData a) => Int -> Int -> String -> Specification a -> Benchmark
benchSpec seed size nm spec =
  bench (unlines [nm, show (genFromSpecWithSeed seed size spec)]) $
    nf (genFromSpecWithSeed seed size) spec
