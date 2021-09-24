{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Shelley.Rules.TestPool
  ( poolRegistration,
    poolStateIsInternallyConsistent,
    poolRetirement,
  )
where

import Cardano.Ledger.Shelley.Delegation.Certificates (PoolCert (RegPool, RetirePool))
import Cardano.Ledger.Shelley.LedgerState
  ( PState (..),
    _fPParams,
    _pParams,
  )
import Cardano.Ledger.Shelley.Rules.Pool (POOL)
import Cardano.Ledger.Shelley.TxBody (DCert (DCertPool), PoolParams (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.SetAlgebra (dom, eval, (∈), (∉))
import Control.State.Transition.Trace
  ( SourceSignalTarget (..),
  )
import qualified Data.Map.Strict as Map (keysSet, lookup)
import qualified Data.Set as Set (isSubsetOf)
import Test.QuickCheck (Property, conjoin, counterexample, property, (===))

poolRegistration :: SourceSignalTarget (POOL era) -> Property
poolRegistration
  SourceSignalTarget
    { signal = (DCertPool (RegPool poolParams)),
      source = sourceSt,
      target = targetSt
    } =
    let hk = _poolId poolParams
        reRegistration = eval (hk ∈ dom (_pParams sourceSt))
     in if reRegistration
          then
            conjoin
              [ counterexample
                  "Pre-existing PoolParams must still be registered in pParams"
                  ((eval (hk ∈ dom (_pParams targetSt))) :: Bool),
                counterexample
                  "New PoolParams are registered in future Params map"
                  (Map.lookup hk (_fPParams targetSt) === Just poolParams),
                counterexample
                  "PoolParams are removed in 'retiring'"
                  ((eval (hk ∉ dom (_retiring targetSt))) :: Bool)
              ]
          else -- first registration

            conjoin
              [ counterexample
                  "New PoolParams are registered in pParams"
                  (Map.lookup hk (_pParams targetSt) === Just poolParams),
                counterexample
                  "PoolParams are not present in 'future pool params'"
                  ((eval (hk ∉ dom (_fPParams targetSt))) :: Bool),
                counterexample
                  "PoolParams are removed in 'retiring'"
                  ((eval (hk ∉ dom (_retiring targetSt))) :: Bool)
              ]
poolRegistration _ = property ()

poolRetirement :: EpochNo -> EpochNo -> SourceSignalTarget (POOL era) -> Property
poolRetirement
  currentEpoch@(EpochNo ce)
  (EpochNo maxEpoch)
  (SourceSignalTarget {source = sourceSt, target = targetSt, signal = (DCertPool (RetirePool hk e))}) =
    conjoin
      [ counterexample
          ("epoch must be well formed " <> show ce <> " " <> show e <> " " <> show maxEpoch)
          (currentEpoch < e && e < EpochNo (ce + maxEpoch)),
        counterexample
          "hk must be in source stPools"
          ((eval (hk ∈ dom (_pParams sourceSt))) :: Bool),
        counterexample
          "hk must be in target stPools"
          ((eval (hk ∈ dom (_pParams targetSt))) :: Bool),
        counterexample
          "hk must be in target's retiring"
          ((eval (hk ∈ dom (_retiring targetSt))) :: Bool)
      ]
poolRetirement _ _ _ = property ()

poolStateIsInternallyConsistent :: PState crypto -> Property
poolStateIsInternallyConsistent (PState pParams_ _ retiring_) = do
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
