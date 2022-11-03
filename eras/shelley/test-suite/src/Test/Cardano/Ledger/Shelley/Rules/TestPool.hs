{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.TestPool
  ( poolRegistration,
    poolStateIsInternallyConsistent,
    poolRetirement,
  )
where

import Cardano.Ledger.Shelley.Delegation.Certificates (PoolCert (RegPool, RetirePool))
import Cardano.Ledger.Shelley.LedgerState
  ( PState (..),
    psFutureStakePoolParams,
    psStakePoolParams,
  )
import Cardano.Ledger.Shelley.Rules (ShelleyPOOL)
import Cardano.Ledger.Shelley.TxBody (DCert (DCertPool), PoolParams (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.SetAlgebra (dom, eval, (∈), (∉))
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
  )
import qualified Data.Map.Strict as Map (keysSet, lookup)
import qualified Data.Set as Set (isSubsetOf)
import Test.QuickCheck (Property, conjoin, counterexample, property, (===))

poolRegistration :: SourceSignalTarget (ShelleyPOOL era) -> Property
poolRegistration
  SourceSignalTarget
    { signal = (DCertPool (RegPool poolParams)),
      source = sourceSt,
      target = targetSt
    } =
    let hk = ppId poolParams
        reRegistration = eval (hk ∈ dom (psStakePoolParams sourceSt))
     in if reRegistration
          then
            conjoin
              [ counterexample
                  "Pre-existing PoolParams must still be registered in pParams"
                  (eval (hk ∈ dom (psStakePoolParams targetSt)) :: Bool),
                counterexample
                  "New PoolParams are registered in future Params map"
                  (Map.lookup hk (psFutureStakePoolParams targetSt) === Just poolParams),
                counterexample
                  "PoolParams are removed in 'retiring'"
                  (eval (hk ∉ dom (psRetiring targetSt)) :: Bool)
              ]
          else -- first registration

            conjoin
              [ counterexample
                  "New PoolParams are registered in pParams"
                  (Map.lookup hk (psStakePoolParams targetSt) === Just poolParams),
                counterexample
                  "PoolParams are not present in 'future pool params'"
                  (eval (hk ∉ dom (psFutureStakePoolParams targetSt)) :: Bool),
                counterexample
                  "PoolParams are removed in 'retiring'"
                  (eval (hk ∉ dom (psRetiring targetSt)) :: Bool)
              ]
poolRegistration _ = property ()

poolRetirement :: EpochNo -> EpochNo -> SourceSignalTarget (ShelleyPOOL era) -> Property
poolRetirement
  currentEpoch@(EpochNo ce)
  (EpochNo maxEpoch)
  SourceSignalTarget {source = sourceSt, target = targetSt, signal = (DCertPool (RetirePool hk e))} =
    conjoin
      [ counterexample
          ("epoch must be well formed " <> show ce <> " " <> show e <> " " <> show maxEpoch)
          (currentEpoch < e && e < EpochNo (ce + maxEpoch)),
        counterexample
          "hk must be in source stPools"
          (eval (hk ∈ dom (psStakePoolParams sourceSt)) :: Bool),
        counterexample
          "hk must be in target stPools"
          (eval (hk ∈ dom (psStakePoolParams targetSt)) :: Bool),
        counterexample
          "hk must be in target's retiring"
          (eval (hk ∈ dom (psRetiring targetSt)) :: Bool)
      ]
poolRetirement _ _ _ = property ()

poolStateIsInternallyConsistent :: PState c -> Property
poolStateIsInternallyConsistent PState {psStakePoolParams = pParams_, psRetiring = retiring_} = do
  let poolKeys = Map.keysSet pParams_
      pParamKeys = Map.keysSet pParams_
      retiringKeys = Map.keysSet retiring_

  conjoin
    [ counterexample
        "All pool keys should be in both stPools and pParams"
        (poolKeys === pParamKeys),
      counterexample
        "A retiring pool should still be registered in `stPools`"
        ((retiringKeys `Set.isSubsetOf` poolKeys) === True)
    ]
