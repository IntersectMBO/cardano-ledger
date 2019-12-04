{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPool where

import           Control.Monad (when)
import           Data.Foldable (traverse_)
import           Data.Map (Map, (!?))
import qualified Data.Map as M
import qualified Data.Maybe as Maybe (maybe)
import qualified Data.Set as S
import           Data.Word (Word64)
import           Lens.Micro (to, (^.))
import           Hedgehog (MonadTest, Property, assert, forAll, property, withTests, (===))
import qualified Test.QuickCheck as QC
import           Control.State.Transition (Environment, State)
import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, sourceSignalTargets, target, _traceEnv)

import           BaseTypes ((==>))
import           Delegation.Certificates (cwitness)
import           LedgerState (pattern PState, pParams, retiring, stPools, _retiring, _stPools)
import           MockTypes (KeyHash, LEDGER, POOL, PState, PoolParams, StakePools)
import           PParams (_eMax)
import           Slot (EpochNo (..))
import           STS.Ledger (LedgerEnv (ledgerSlotNo))
import           STS.Pool (PoolEnv (..))
import           TxData (pattern KeyHashObj, pattern RegPool, pattern RetirePool,
                     pattern StakePools, poolPubKey)


import           Ledger.Core (dom, (∈), (∉))
import           Test.Utils (assertAll, testGlobals, epochFromSlotNo)

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
rewardZeroAfterReg :: Property
rewardZeroAfterReg = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
        $ forAll
        $ trace @POOL testGlobals traceLen `ofLengthAtLeast` 1

  assertAll registeredPoolNotRetiring tr

  where registeredPoolNotRetiring (SourceSignalTarget
                                    { signal = c@(RegPool _)
                                    , target = p'}) =
          case cwitness c of
            KeyHashObj certWit -> let StakePools stp = getStPools p' in
                                      (  certWit ∈ dom stp
                                      && certWit ∉ dom (getRetiring p'))
            _                  -> False
        registeredPoolNotRetiring _ = True

-- | Check that if a pool retirement certificate is executed in the correct
-- epoch interval, then the pool key will be added to the retiring map but stays
-- in the set of stake pools.
poolRetireInEpoch :: Property
poolRetireInEpoch = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @POOL testGlobals traceLen `ofLengthAtLeast` 1)
  let
    tr = sourceSignalTargets t
    PoolEnv s pp = _traceEnv t

  assertAll (registeredPoolRetired s pp) tr

  where registeredPoolRetired s pp (SourceSignalTarget
                                    { source = p
                                    , target = p'
                                    , signal = c@(RetirePool _ e)}) =
          case cwitness c of
            KeyHashObj certWit -> let StakePools stp  = getStPools p
                                      StakePools stp' = getStPools p'
                                      cepoch          = epochFromSlotNo s
                                      EpochNo ce        = cepoch
                                      EpochNo emax'     = _eMax pp in
                                    (  cepoch < e
                                    && e < EpochNo (ce + emax')) ==>
                                    (  certWit ∈ dom stp
                                    && certWit ∈ dom stp'
                                    && Maybe.maybe False ((== e) . epochFromSlotNo) (stp' !? certWit))
            _                  -> False
        registeredPoolRetired _ _ _ = True

-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded
  :: forall m
   . MonadTest m
  => Environment LEDGER
  -> [SourceSignalTarget POOL]
  -> m ()
registeredPoolIsAdded env ssts =
  mapM_ addedRegPool ssts

 where

  addedRegPool :: SourceSignalTarget POOL
               -> m ()
  addedRegPool sst =
    case signal sst of
      RegPool poolParams -> check poolParams
      _ -> pure ()
   where
    check :: PoolParams -> m ()
    check poolParams = do
      let hk = poolParams ^. poolPubKey
          sSt = source sst
          tSt = target sst
      -- Check for pool re-registration. If we register a pool which was already
      -- registered (indicated by presence in `stPools`), then we check that it
      -- is not in `retiring` after the signal has been processed.
      when (hk ∈ dom (sSt ^. stPools . to (\(StakePools x) -> x))) $ do
        assert (hk ∈ dom (sSt ^. retiring))
        assert (hk ∉ dom (tSt ^. retiring))
      -- PoolParams are registered in pParams map
      M.lookup hk (tSt ^. pParams) === Just poolParams
      -- Hashkey is registered in stPools map
      M.lookup hk (tSt ^. stPools . to (\(StakePools x) -> x))
        === Just (ledgerSlotNo env)

-- | Check that a `RetirePool` certificate properly marks a stake pool for
-- retirement.
poolIsMarkedForRetirement
  :: [SourceSignalTarget POOL]
  -> QC.Property
poolIsMarkedForRetirement ssts =
  QC.conjoin (map check ssts)
 where
  check :: SourceSignalTarget POOL
        -> QC.Property
  check sst =
    case signal sst of
      -- We omit a well-formedness check for `epoch`, because the executable
      -- spec will throw a PredicateFailure in that case.
      RetirePool hk _epoch -> wasRemoved hk
      _ -> QC.property ()
   where
    wasRemoved :: KeyHash -> QC.Property
    wasRemoved hk = QC.conjoin
      [ QC.counterexample "hk not in stPools"
          (hk ∈ dom (source sst ^. (stPools . to (\(StakePools x) -> x))))
      , QC.counterexample "hk is not in target's retiring"
          (hk ∈ dom (target sst ^. retiring))
      ]

-- | Assert that PState maps are in sync with each other after each `Signal
-- POOL` transition.
pStateIsInternallyConsistent
  :: forall m
   . MonadTest m
  => [SourceSignalTarget POOL]
  -> m ()
pStateIsInternallyConsistent ssts =
  traverse_ isConsistent (concatMap (\sst -> [source sst, target sst]) ssts)
 where
  isConsistent :: State POOL -> m ()
  isConsistent (PState stPools_ pParams_ retiring_) = do
    let StakePools stPoolsMap = stPools_
        poolKeys = M.keysSet stPoolsMap
        pParamKeys = M.keysSet pParams_
        retiringKeys = M.keys retiring_

    sequence_ [ -- These 2 key sets should be equal.
                poolKeys === pParamKeys
                -- A retiring pool should still be registered in `stPools`.
              , traverse_ (assert . (`S.member` poolKeys)) retiringKeys
              ]
