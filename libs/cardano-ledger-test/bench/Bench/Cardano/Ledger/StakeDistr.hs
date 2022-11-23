{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Benchmark the TickF rule. and its major sub-computations and rules, to discover where the time is spent
module Bench.Cardano.Ledger.StakeDistr
  ( tickfRuleBench,
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD (..))
import Cardano.Ledger.BaseTypes (BlocksMade (..), Globals (..), pvMajor)
import Cardano.Ledger.Binary (FromCBOR (..), decodeFullDecoder)
import Cardano.Ledger.Coin (DeltaCoin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.EpochBoundary (SnapShot (..), SnapShots (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..), mkShelleyGlobals)
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState (..),
    FilteredRewards (..),
    NewEpochState (..),
    PulsingRewUpdate (Complete, Pulsing),
    RewardUpdate (..),
    applyRUpdFiltered,
    dpsDState,
    filterAllRewards,
    lsDPState,
    rewards,
  )
import Cardano.Ledger.Shelley.Rewards (aggregateRewards, sumRewards)
import Cardano.Ledger.Shelley.Rules
  ( ShelleyEPOCH,
    ShelleyMIR,
    ShelleyNEWEPOCH,
    ShelleyTICKF,
    adoptGenesisDelegs,
    calculatePoolDistr,
    calculatePoolStake,
    updateRewards,
    validatingTickTransition,
  )
import Cardano.Ledger.Slot (EpochNo, SlotNo (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo)
import Cardano.Slotting.Time (mkSlotLength)
import Control.Monad.Reader (Reader, runReader)
import Control.State.Transition.Extended (Rule, RuleContext, RuleType (Transition), STS (..), TRC (TRC), applySTS, runRule)
import Criterion (Benchmark, bench, bgroup, env, nf, whnf)
import qualified Data.Aeson as Aeson (eitherDecode)
import Data.ByteString.Lazy as Lazy (readFile)
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Text (pack)
import qualified Data.UMap as UM
import System.Environment (lookupEnv)
import Test.Cardano.Ledger.Tickf (oldCalculatePoolDistr)

-- ======================================================================

type CurrentEra = AlonzoEra StandardCrypto

-- ==============================
-- Data files
-- We will need to deserialize a CBOR file that can be deserialized to a NewEpochState.
-- We use the environment variable  "BENCH_LEDGER_STATE_PATH" to indicate where to get this CBOR file.
-- If the file does not  exist at this path, the benchmark will run, but be meaningless.
-- See the README.md file in ledger-state/libs/ledger-state/README.md for instructions for
-- how one might obtain such a file. Get your own copy and adjust the path here.
--
-- One way to run the benchmark would be like this
-- BENCH_LEDGER_STATE_PATH='/home/sheard/Two/Data/71452796-NewEpochState.cbor' cabal bench bench
-- Of course replace '/home/sheard/Two/Data/71452796-NewEpochState.cbor'  with your own path.

-- ==============================
-- Stored constants read from files. Globals and NewEpochState

readGlobals :: IO Globals
readGlobals = pure $ mkGlobals shelleyGenesis pp
  where
    pp = def

readNewEpochState :: IO (NewEpochState CurrentEra)
readNewEpochState = do
  let ledgerVarName = "BENCH_LEDGER_STATE_PATH"
  filePath <- lookupEnv ledgerVarName
  case filePath of
    Just ledgerStateFilePath -> do
      lazyBytes <- Lazy.readFile ledgerStateFilePath
      let lbl = "NewEpochState from " <> pack ledgerStateFilePath
      case decodeFullDecoder (eraProtVerHigh @CurrentEra) lbl fromCBOR lazyBytes of
        Left err -> error (show err)
        Right (nes :: NewEpochState CurrentEra) -> pure nes
    Nothing ->
      bogusNewEpochState <$ do
        putStrLn $
          unlines
            [ "No path to the NewEpochState CBOR was provided",
              "The benchmark results are meaningless."
            ]

bogusNewEpochState :: NewEpochState CurrentEra
bogusNewEpochState =
  NewEpochState
    (EpochNo 0)
    (BlocksMade Map.empty)
    (BlocksMade Map.empty)
    def
    (SJust (Complete (RewardUpdate (DeltaCoin 0) (DeltaCoin 0) def (DeltaCoin 0) def)))
    (PoolDistr Map.empty)
    def

mkGlobals :: ShelleyGenesis CurrentEra -> PParams CurrentEra -> Globals
mkGlobals genesis pp =
  mkShelleyGlobals genesis epochInfoE majorPParamsVer
  where
    majorPParamsVer = pvMajor $ _protocolVersion pp
    epochInfoE =
      fixedEpochInfo
        (sgEpochLength genesis)
        (mkSlotLength $ sgSlotLength genesis)

-- =========================================================
-- We would like to benchmark things that are used in the STS system.
-- Two things come to mind
-- 1) calls of applySTS
-- 2) things of type (STS s => Rule s 'Transition t)
-- To do this we need to lift them to values not embedded in STS things.

-- | usd to run an action made by applying 'runRule' to an 'Rule' and a 'RuleContext'
liftRule :: (STS s, BaseM s ~ Reader r) => r -> RuleContext 'Transition s -> Rule s 'Transition p -> p
liftRule globals context rule =
  case flip runReader globals (runRule context rule) of
    (es, []) -> es
    (_, fails) -> error (show fails)

-- | Used to run an 'action' which is an 'applySTS' call
liftApplySTS :: Show a => r -> Reader r (Either a p) -> p
liftApplySTS globals action =
  case flip runReader globals action of
    Left fails -> error (show fails)
    Right x -> x

-- =======================================

updateRewardsX :: Globals -> NewEpochState CurrentEra -> EpochState CurrentEra
updateRewardsX globals newepochstate =
  let epochNo = nesEL newepochstate
      epochstate = nesEs newepochstate
      context = (TRC ((), newepochstate, epochNo))
      rule = case nesRu newepochstate of
        SNothing -> pure epochstate
        SJust p -> updateRewards epochstate epochNo $ getComplete p
   in liftRule globals context rule

adoptGenesisDelegsR ::
  Cardano.Slotting.Slot.SlotNo ->
  NewEpochState era ->
  EpochState era
adoptGenesisDelegsR slot nes = adoptGenesisDelegs (nesEs nes) slot

tickfR2 ::
  Globals ->
  Cardano.Slotting.Slot.SlotNo ->
  NewEpochState CurrentEra ->
  NewEpochState CurrentEra
tickfR2 globals slot nes = liftRule globals (TRC ((), nes, slot)) (validatingTickTransition @ShelleyTICKF nes slot)

mirR :: Globals -> EpochState CurrentEra -> EpochState CurrentEra
mirR globals es' = liftApplySTS globals (applySTS @(ShelleyMIR CurrentEra) (TRC ((), es', ())))

newEpochR :: Globals -> EpochNo -> NewEpochState CurrentEra -> NewEpochState CurrentEra
newEpochR globals epochNo nes = liftApplySTS globals (applySTS @(ShelleyNEWEPOCH CurrentEra) (TRC ((), nes, epochNo)))

epochR :: Globals -> EpochNo -> EpochState CurrentEra -> EpochState CurrentEra
epochR globals epochNo es'' = liftApplySTS globals (applySTS @(ShelleyEPOCH CurrentEra) (TRC ((), es'', epochNo)))

-- ============================================================

tickfRuleBench :: Benchmark
tickfRuleBench =
  env readGlobals $ \globals ->
    env readNewEpochState $ \nes ->
      bgroup
        "Tickf Benchmarks"
        [ bench "validatingTickTransitionfunction" $ whnf (tickfR2 globals (SlotNo 156953303)) nes,
          bgroup
            "Tick subparts"
            [ bench "adoptGenesisDelegs" $ whnf (adoptGenesisDelegsR (SlotNo 156953303)) nes,
              bench "newEpoch" $ whnf (newEpochR globals (nesEL nes + 1)) nes,
              bgroup
                "NewEpoch sub parts"
                [ bench "updateRewards" $ whnf (updateRewardsX globals) nes,
                  bgroup
                    "updateRewards subparts"
                    [ bench "sumRewards" $ whnf (sumRewards (esPp (nesEs nes))) (rs (getRewardUpdate nes)),
                      bench "applyRUpdFiltered Incremental" $ whnf (applyRUpdFiltered (getRewardUpdate nes)) (nesEs nes),
                      bgroup
                        "applyRUpd subparts"
                        [ bench "filterAllRewards" $ nf (filterAllRewards (rs (getRewardUpdate nes))) (nesEs nes),
                          env
                            (pure (filterAllRewards (rs (getRewardUpdate nes)) (nesEs nes)))
                            (bench "aggregateRewards" . whnf (aggregateRewards (esPp (nesEs nes))) . frRegistered),
                          env
                            ( pure
                                ( aggregateRewards
                                    (esPp (nesEs nes))
                                    ( frRegistered $
                                        filterAllRewards (rs (getRewardUpdate nes)) (nesEs nes)
                                    )
                                )
                            )
                            ( \registeredAggregated ->
                                bench "union+" $
                                  let dState = (dpsDState . lsDPState . esLState . nesEs) nes
                                   in whnf (rewards dState UM.∪+) registeredAggregated
                            )
                        ]
                    ],
                  bench "mir rule" $ whnf (mirR globals) (nesEs nes),
                  bench "epoch rule" $ whnf (epochR globals (nesEL nes + 1)) (nesEs nes),
                  bench "calculatePoolDistr, improved (current) version" $ whnf calculatePoolDistr (getSnap nes),
                  bgroup
                    "calculatePoolDistr subparts"
                    [ bench "poolStake" $
                        whnf (calculatePoolStake (const True) (ssDelegations (getSnap nes))) (ssStake (getSnap nes)),
                      bench "old calculatePoolDistr" $
                        whnf (oldCalculatePoolDistr (const True)) (getSnap nes)
                    ]
                ]
            ]
        ]

getSnap :: NewEpochState (AlonzoEra StandardCrypto) -> SnapShot StandardCrypto
getSnap nes = (ssStakeSet . esSnapshots . nesEs) nes

getRewardUpdate :: NewEpochState era -> RewardUpdate (EraCrypto era)
getRewardUpdate nes =
  case nesRu nes of
    SNothing -> error "No RewardUpdate in new epoch state"
    SJust c -> getComplete c

getComplete :: PulsingRewUpdate c -> RewardUpdate c
getComplete = \case
  Pulsing _ _ -> error "RewardUpdate in new epoch state is pulsing"
  Complete ru -> ru

-- ==================================================================

-- | The inital ShelleyGenesis structure, encoded in json format as a Haskell String
shelleyGenesis :: ShelleyGenesis CurrentEra
shelleyGenesis =
  either error id $
    Aeson.eitherDecode $
      "{\
      \  \"activeSlotsCoeff\": 0.05,\
      \  \"protocolParams\": {\
      \    \"protocolVersion\": {\
      \      \"minor\": 0,\
      \      \"major\": 2\
      \    },\
      \    \"decentralisationParam\": 1,\
      \    \"eMax\": 18,\
      \    \"extraEntropy\": {\
      \      \"tag\": \"NeutralNonce\"\
      \    },\
      \    \"maxTxSize\": 16384,\
      \    \"maxBlockBodySize\": 65536,\
      \    \"maxBlockHeaderSize\": 1100,\
      \    \"minFeeA\": 44,\
      \    \"minFeeB\": 155381,\
      \    \"minUTxOValue\": 1000000,\
      \    \"poolDeposit\": 500000000,\
      \    \"minPoolCost\": 340000000,\
      \    \"keyDeposit\": 2000000,\
      \    \"nOpt\": 150,\
      \    \"rho\": 0.003,\
      \    \"tau\": 0.20,\
      \    \"a0\": 0.3\
      \  },\
      \  \"genDelegs\": {\
      \    \"ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c\": {\
      \      \"delegate\": \"d9e5c76ad5ee778960804094a389f0b546b5c2b140a62f8ec43ea54d\",\
      \      \"vrf\": \"64fa87e8b29a5b7bfbd6795677e3e878c505bc4a3649485d366b50abadec92d7\"\
      \    },\
      \    \"b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497\": {\
      \      \"delegate\": \"855d6fc1e54274e331e34478eeac8d060b0b90c1f9e8a2b01167c048\",\
      \      \"vrf\": \"66d5167a1f426bd1adcc8bbf4b88c280d38c148d135cb41e3f5a39f948ad7fcc\"\
      \    },\
      \    \"60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1\": {\
      \      \"delegate\": \"7f72a1826ae3b279782ab2bc582d0d2958de65bd86b2c4f82d8ba956\",\
      \      \"vrf\": \"c0546d9aa5740afd569d3c2d9c412595cd60822bb6d9a4e8ce6c43d12bd0f674\"\
      \    },\
      \    \"f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757\": {\
      \      \"delegate\": \"69ae12f9e45c0c9122356c8e624b1fbbed6c22a2e3b4358cf0cb5011\",\
      \      \"vrf\": \"6394a632af51a32768a6f12dac3485d9c0712d0b54e3f389f355385762a478f2\"\
      \    },\
      \    \"162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82\": {\
      \      \"delegate\": \"4485708022839a7b9b8b639a939c85ec0ed6999b5b6dc651b03c43f6\",\
      \      \"vrf\": \"aba81e764b71006c515986bf7b37a72fbb5554f78e6775f08e384dbd572a4b32\"\
      \    },\
      \    \"2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6\": {\
      \      \"delegate\": \"6535db26347283990a252313a7903a45e3526ec25ddba381c071b25b\",\
      \      \"vrf\": \"fcaca997b8105bd860876348fc2c6e68b13607f9bbd23515cd2193b555d267af\"\
      \    },\
      \    \"268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b\": {\
      \      \"delegate\": \"1d4f2e1fda43070d71bb22a5522f86943c7c18aeb4fa47a362c27e23\",\
      \      \"vrf\": \"63ef48bc5355f3e7973100c371d6a095251c80ceb40559f4750aa7014a6fb6db\"\
      \    }\
      \  },\
      \  \"updateQuorum\": 5,\
      \  \"networkId\": \"Mainnet\",\
      \  \"initialFunds\": {},\
      \  \"maxLovelaceSupply\": 45000000000000000,\
      \  \"networkMagic\": 764824073,\
      \  \"epochLength\": 432000,\
      \  \"systemStart\": \"2017-09-23T21:44:51Z\",\
      \  \"slotsPerKESPeriod\": 129600,\
      \  \"slotLength\": 1,\
      \  \"maxKESEvolutions\": 62,\
      \  \"securityParam\": 2160\
      \}"
