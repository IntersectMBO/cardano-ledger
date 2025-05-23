{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.Pool (
  tests,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Block (bheader)
import Cardano.Ledger.Core
import Cardano.Ledger.PoolParams (ppId)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState (..),
  curPParamsEpochStateL,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyPOOL)
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Protocol.TPraos.BHeader (bhbody, bheaderSlotNo)
import Control.SetAlgebra (dom, eval, (∈), (∉))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainState (..))
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  forAllChainTrace,
  poolTraceFromBlock,
  traceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
  epochFromSlotNo,
 )
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  TraceOrder (OldestFirst),
  sourceSignalTargets,
  traceStates,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- | Various properties of the POOL STS Rule, tested on longer traces
-- (double the default length)
tests ::
  forall era.
  ( EraGen era
  , EraStake era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  TestTree
tests =
  testProperty "properties of the POOL STS" $
    forAllChainTrace @era traceLen defaultConstants $ \tr -> do
      let ssts = sourceSignalTargets tr
      conjoin . concat $
        [ map poolRetirement ssts
        , map poolRegistration ssts
        , map poolStateIsInternallyConsistent ssts
        ]

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolRetirement ::
  ( ChainProperty era
  , ShelleyEraTxBody era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRetirement SourceSignalTarget {source = chainSt, signal = block} =
  conjoin $
    map (poolRetirementProp currentEpoch maxEpoch) (sourceSignalTargets poolTr)
  where
    (chainSt', poolTr) = poolTraceFromBlock chainSt block
    bhb = bhbody $ bheader block
    currentEpoch = (epochFromSlotNo . bheaderSlotNo) bhb
    maxEpoch = (view ppEMaxL . view curPParamsEpochStateL . nesEs . chainNes) chainSt'

-- | Check that a newly registered pool key is registered and not
-- in the retiring map.
poolRegistration ::
  ( ChainProperty era
  , ShelleyEraTxBody era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolRegistration (SourceSignalTarget {source = chainSt, signal = block}) =
  conjoin $
    map poolRegistrationProp (sourceSignalTargets poolTr)
  where
    (_, poolTr) = poolTraceFromBlock chainSt block

-- | Assert that PState maps are in sync with each other after each `Signal
-- POOL` transition.
poolStateIsInternallyConsistent ::
  ( ChainProperty era
  , ShelleyEraTxBody era
  ) =>
  SourceSignalTarget (CHAIN era) ->
  Property
poolStateIsInternallyConsistent (SourceSignalTarget {source = chainSt, signal = block}) =
  conjoin $
    map poolStateIsInternallyConsistentProp (traceStates OldestFirst poolTr)
  where
    (_, poolTr) = poolTraceFromBlock chainSt block

poolRegistrationProp :: SourceSignalTarget (ShelleyPOOL era) -> Property
poolRegistrationProp
  SourceSignalTarget
    { signal = RegPool poolParams
    , source = sourceSt
    , target = targetSt
    } =
    let hk = ppId poolParams
        reRegistration = eval (hk ∈ dom (psStakePoolParams sourceSt))
     in if reRegistration
          then
            conjoin
              [ counterexample
                  "Pre-existing PoolParams must still be registered in pParams"
                  (eval (hk ∈ dom (psStakePoolParams targetSt)) :: Bool)
              , counterexample
                  "New PoolParams are registered in future Params map"
                  (Map.lookup hk (psFutureStakePoolParams targetSt) === Just poolParams)
              , counterexample
                  "PoolParams are removed in 'retiring'"
                  (eval (hk ∉ dom (psRetiring targetSt)) :: Bool)
              ]
          else -- first registration
            conjoin
              [ counterexample
                  "New PoolParams are registered in pParams"
                  (Map.lookup hk (psStakePoolParams targetSt) === Just poolParams)
              , counterexample
                  "PoolParams are not present in 'future pool params'"
                  (eval (hk ∉ dom (psFutureStakePoolParams targetSt)) :: Bool)
              , counterexample
                  "PoolParams are removed in 'retiring'"
                  (eval (hk ∉ dom (psRetiring targetSt)) :: Bool)
              ]
poolRegistrationProp _ = property ()

poolRetirementProp ::
  EpochNo ->
  EpochInterval ->
  SourceSignalTarget (ShelleyPOOL era) ->
  Property
poolRetirementProp
  currentEpoch@(EpochNo ce)
  (EpochInterval maxEpoch)
  SourceSignalTarget {source = sourceSt, target = targetSt, signal = RetirePool hk e} =
    conjoin
      [ counterexample
          ("epoch must be well formed " <> show ce <> " " <> show e <> " " <> show maxEpoch)
          (currentEpoch < e && e < EpochNo (ce + fromIntegral maxEpoch))
      , counterexample
          "hk must be in source stPools"
          (eval (hk ∈ dom (psStakePoolParams sourceSt)) :: Bool)
      , counterexample
          "hk must be in target stPools"
          (eval (hk ∈ dom (psStakePoolParams targetSt)) :: Bool)
      , counterexample
          "hk must be in target's retiring"
          (eval (hk ∈ dom (psRetiring targetSt)) :: Bool)
      ]
poolRetirementProp _ _ _ = property ()

poolStateIsInternallyConsistentProp :: PState c -> Property
poolStateIsInternallyConsistentProp PState {psStakePoolParams = pParams_, psRetiring = retiring_} = do
  let poolKeys = Map.keysSet pParams_
      pParamKeys = Map.keysSet pParams_
      retiringKeys = Map.keysSet retiring_

  conjoin
    [ counterexample
        "All pool keys should be in both stPools and pParams"
        (poolKeys === pParamKeys)
    , counterexample
        "A retiring pool should still be registered in `stPools`"
        ((retiringKeys `Set.isSubsetOf` poolKeys) === True)
    ]
