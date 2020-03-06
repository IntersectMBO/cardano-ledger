{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPool where

import           Data.Map (Map, (!?))
import qualified Data.Map as M
import qualified Data.Maybe as Maybe (maybe)
import qualified Data.Set as S
import           Data.Word (Word64)
import           Lens.Micro (to, (^.))

import           Test.QuickCheck (Property, conjoin, counterexample, property, (===))

import           Control.State.Transition (Environment, State)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, target)

import           ConcreteCryptoTypes (KeyHash, LEDGER, POOL, PState, PoolParams, StakePools)
import           Shelley.Spec.Ledger.BaseTypes ((==>))
import           Shelley.Spec.Ledger.Delegation.Certificates (poolCWitness)
import           Shelley.Spec.Ledger.LedgerState (pattern PState, pParams, retiring, stPools,
                     _retiring, _stPools)
import           Shelley.Spec.Ledger.PParams (_eMax)
import           Shelley.Spec.Ledger.Slot (EpochNo (..))
import           Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (ledgerPp, ledgerSlotNo))
import           Shelley.Spec.Ledger.TxData (pattern DCertPool, pattern KeyHashObj, pattern RegPool,
                     pattern RetirePool, pattern StakePools, poolPubKey)

import           Ledger.Core (dom, (∈), (∉))
import           Test.Utils (epochFromSlotNo)

-------------------------------
-- helper accessor functions --
-------------------------------

getRetiring :: PState -> Map KeyHash EpochNo
getRetiring = _retiring

getStPools :: PState -> StakePools
getStPools = _stPools

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
rewardZeroAfterReg
  :: [SourceSignalTarget POOL]
  -> Property
rewardZeroAfterReg ssts =
  conjoin $
    map registeredPoolNotRetiring ssts
  where
    registeredPoolNotRetiring SourceSignalTarget
                                { signal = (DCertPool c@(RegPool _))
                                , target = p'} =
      case poolCWitness c of
        KeyHashObj certWit -> let StakePools stp = getStPools p' in
                                  (  certWit ∈ dom stp
                                  && certWit ∉ dom (getRetiring p'))
        _                  -> False
    registeredPoolNotRetiring _ = True

-- | Check that if a pool retirement certificate is executed in the correct
-- epoch interval, then the pool key will be added to the retiring map but stays
-- in the set of stake pools.
poolRetireInEpoch
  :: Environment LEDGER
  -> [SourceSignalTarget POOL]
  -> Property
poolRetireInEpoch env ssts =
  conjoin $
    map (registeredPoolRetired (ledgerSlotNo env) (ledgerPp env)) ssts
  where
    registeredPoolRetired s pp SourceSignalTarget
                                { source = p
                                , target = p'
                                , signal = (DCertPool c@(RetirePool _ e))} =
      case poolCWitness c of
        KeyHashObj certWit -> let StakePools stp  = getStPools p
                                  StakePools stp' = getStPools p'
                                  cepoch          = epochFromSlotNo s
                                  EpochNo ce      = cepoch
                                  EpochNo emax'   = _eMax pp in
                                (  cepoch < e
                                && e < EpochNo (ce + emax')) ==>
                                (  certWit ∈ dom stp
                                && certWit ∈ dom stp'
                                && Maybe.maybe False ((== cepoch) . epochFromSlotNo) (stp' !? certWit))
        _                  -> False
    registeredPoolRetired _ _ _ = True

-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded
  :: Environment LEDGER
  -> [SourceSignalTarget POOL]
  -> Property
registeredPoolIsAdded env ssts =
  conjoin $
    map addedRegPool ssts
 where
  addedRegPool :: SourceSignalTarget POOL
               -> Property
  addedRegPool sst =
    case signal sst of
      DCertPool (RegPool poolParams) -> check poolParams
      _ -> property ()
   where
    check :: PoolParams -> Property
    check poolParams = do
      let hk = poolParams ^. poolPubKey
          sSt = source sst
          tSt = target sst

      conjoin
        [ -- Check for pool re-registration. If we register a pool which was already
          -- registered (indicated by presence in `stPools`), then we check that it
          -- is not in `retiring` after the signal has been processed.
          if hk ∈ dom (sSt ^. stPools . to (\(StakePools x) -> x))
            then
              conjoin
                [ counterexample "Pool re-registration: pool should be in 'retiring' before signal"
                    (hk ∈ dom (sSt ^. retiring))
                , counterexample "Pool re-registration: pool should not be in 'retiring' after signal"
                    (hk ∉ dom (tSt ^. retiring))
                ]
            else property ()

        , counterexample "PoolParams are registered in pParams map"
            (M.lookup hk (tSt ^. pParams) === Just poolParams)

        , counterexample "Hashkey is registered in stPools map"
            (M.lookup hk (tSt ^. stPools . to (\(StakePools x) -> x))
               === Just (ledgerSlotNo env))
        ]

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolIsMarkedForRetirement
  :: [SourceSignalTarget POOL]
  -> Property
poolIsMarkedForRetirement ssts =
  conjoin (map check ssts)
 where
  check :: SourceSignalTarget POOL
        -> Property
  check sst =
    case signal sst of
      -- We omit a well-formedness check for `epoch`, because the executable
      -- spec will throw a PredicateFailure in that case.
      DCertPool (RetirePool hk _epoch) -> wasRemoved hk
      _ -> property ()
   where
    wasRemoved :: KeyHash -> Property
    wasRemoved hk = conjoin
      [ counterexample "hk not in stPools"
          (hk ∈ dom (source sst ^. (stPools . to (\(StakePools x) -> x))))
      , counterexample "hk is not in target's retiring"
          (hk ∈ dom (target sst ^. retiring))
      ]

-- | Assert that PState maps are in sync with each other after each `Signal
-- POOL` transition.
pStateIsInternallyConsistent
  :: [SourceSignalTarget POOL]
  -> Property
pStateIsInternallyConsistent ssts =
  conjoin $
    map isConsistent (concatMap (\sst -> [source sst, target sst]) ssts)
 where
  isConsistent :: State POOL -> Property
  isConsistent (PState stPools_ pParams_ retiring_) = do
    let StakePools stPoolsMap = stPools_
        poolKeys = M.keysSet stPoolsMap
        pParamKeys = M.keysSet pParams_
        retiringKeys = M.keysSet retiring_

    conjoin [ counterexample "All pool keys should be in both stPools and pParams"
                             (poolKeys === pParamKeys)
            , counterexample "A retiring pool should still be registered in `stPools`"
                             ((retiringKeys `S.isSubsetOf` poolKeys) === True)
            ]
