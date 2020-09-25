{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Shelley.Spec.Ledger.Rules.TestPool where

import Control.Iterate.SetAlgebra (dom, eval, (∈), (∉))
import Control.State.Transition (Environment, State)
import Control.State.Transition.Trace
  ( SourceSignalTarget,
    signal,
    source,
    target,
    pattern SourceSignalTarget,
  )
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word (Word64)
import Shelley.Spec.Ledger.API
  ( LEDGER,
    POOL,
  )
import Shelley.Spec.Ledger.BaseTypes ((==>))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates (poolCWitness)
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
  )
import Shelley.Spec.Ledger.LedgerState
  ( PState (..),
  )
import Shelley.Spec.Ledger.PParams (_eMax)
import Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (ledgerPp, ledgerSlotNo))
import Shelley.Spec.Ledger.Slot (EpochNo (..))
import Shelley.Spec.Ledger.TxBody
  ( PoolParams,
    _poolPubKey,
    pattern DCertPool,
    pattern RegPool,
    pattern RetirePool,
  )
import Test.QuickCheck (Property, conjoin, counterexample, property, (===))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( C,
  )
import Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo)

-------------------------------
-- helper accessor functions --
-------------------------------

getRetiring :: PState era -> Map (KeyHash 'StakePool era) EpochNo
getRetiring = _retiring

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

-------------------------
-- Properties for POOL --
-------------------------

-- | Check that a newly registered pool key is not in the retiring map.
rewardZeroAfterReg ::
  [SourceSignalTarget (POOL C)] ->
  Property
rewardZeroAfterReg ssts =
  conjoin $
    map registeredPoolNotRetiring ssts
  where
    registeredPoolNotRetiring
      SourceSignalTarget
        { signal = (DCertPool c@(RegPool _)),
          target = p'
        } =
        case poolCWitness c of
          KeyHashObj certWit ->
            let stp = _pParams p'
             in ( eval (certWit ∈ dom stp)
                    && eval (certWit ∉ dom (getRetiring p'))
                )
          _ -> False
    registeredPoolNotRetiring _ = True

-- | Check that if a pool retirement certificate is executed in the correct
-- epoch interval, then the pool key will be added to the retiring map but stays
-- in the set of stake pools.
poolRetireInEpoch ::
  Environment (LEDGER C) ->
  [SourceSignalTarget (POOL C)] ->
  Property
poolRetireInEpoch env ssts =
  conjoin $
    map (registeredPoolRetired (ledgerSlotNo env) (ledgerPp env)) ssts
  where
    registeredPoolRetired
      s
      pp
      SourceSignalTarget
        { source = p,
          target = p',
          signal = (DCertPool c@(RetirePool _ e))
        } =
        case poolCWitness c of
          KeyHashObj certWit ->
            let stp = _pParams p
                stp' = _pParams p'
                cepoch = epochFromSlotNo s
                EpochNo ce = cepoch
                EpochNo emax' = _eMax pp
             in ( cepoch < e
                    && e < EpochNo (ce + emax')
                )
                  ==> ( eval (certWit ∈ dom stp)
                          && eval (certWit ∈ dom stp')
                      )
          _ -> False
    registeredPoolRetired _ _ _ = True

-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded :: forall era.
  [SourceSignalTarget (POOL era)] ->
  Property
registeredPoolIsAdded ssts =
  conjoin $
    map addedRegPool ssts
  where
    addedRegPool ::
      SourceSignalTarget (POOL era) ->
      Property
    addedRegPool sst =
      case signal sst of
        DCertPool (RegPool poolParams) -> check poolParams
        _ -> property ()
      where
        check :: PoolParams era -> Property
        check poolParams = do
          let hk = _poolPubKey poolParams
              sSt = source sst
              tSt = target sst

          conjoin
            [ -- If this is a pool re-registration (indicated by presence in `stPools`)...
              if eval (hk ∈ dom (_pParams sSt))
                then
                  conjoin
                    [ counterexample
                        "Pool re-registration: pool should not be in 'retiring' after signal"
                        ((eval (hk ∉ dom (_retiring tSt))) :: Bool),
                      counterexample
                        "PoolParams are registered in future Params map"
                        (M.lookup hk (_fPParams tSt) === Just poolParams)
                    ]
                else -- This is the first registration of a pool...

                  conjoin
                    [ counterexample
                        "PoolParams are registered in pParams map"
                        (M.lookup hk (_pParams tSt) === Just poolParams)
                    ]
            ]

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolIsMarkedForRetirement :: forall era.
  [SourceSignalTarget (POOL era)] ->
  Property
poolIsMarkedForRetirement ssts =
  conjoin (map check ssts)
  where
    check ::
      SourceSignalTarget (POOL era) ->
      Property
    check sst =
      case signal sst of
        -- We omit a well-formedness check for `epoch`, because the executable
        -- spec will throw a PredicateFailure in that case.
        DCertPool (RetirePool hk _epoch) -> wasRemoved hk
        _ -> property ()
      where
        wasRemoved :: KeyHash 'StakePool era -> Property
        wasRemoved hk =
          conjoin
            [ counterexample
                "hk not in stPools"
                ((eval (hk ∈ dom (_pParams (source sst)))) :: Bool),
              counterexample
                "hk is not in target's retiring"
                ((eval (hk ∈ dom (_retiring $ target sst))) :: Bool)
            ]

-- | Assert that PState maps are in sync with each other after each `Signal
-- POOL` transition.
pStateIsInternallyConsistent :: forall era.
  [SourceSignalTarget (POOL era)] ->
  Property
pStateIsInternallyConsistent ssts =
  conjoin $
    map isConsistent (concatMap (\sst -> [source sst, target sst]) ssts)
  where
    isConsistent :: State (POOL era) -> Property
    isConsistent (PState pParams_ _ retiring_) = do
      let poolKeys = M.keysSet pParams_
          pParamKeys = M.keysSet pParams_
          retiringKeys = M.keysSet retiring_

      conjoin
        [ counterexample
            "All pool keys should be in both stPools and pParams"
            (poolKeys === pParamKeys),
          counterexample
            "A retiring pool should still be registered in `stPools`"
            ((retiringKeys `S.isSubsetOf` poolKeys) === True)
        ]
