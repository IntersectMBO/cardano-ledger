{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPool where

import           Data.Foldable (toList)
import           Data.Map (Map, (!?))
import qualified Data.Map as M
import qualified Data.Maybe as Maybe (maybe)
import           Data.Word (Word64)
import           Lens.Micro (to, (^.))

import           Hedgehog (MonadTest, Property, forAll, property, withTests)

import           Control.State.Transition (Environment)
import           Control.State.Transition.Generator (ofLengthAtLeast, trace)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     signal, source, sourceSignalTargets, target, _traceEnv)

import           BaseTypes ((==>))
import           Delegation.Certificates (cwitness)
import           LedgerState (pattern DPState, cCounters, pParams, stPools, _retiring, _stPools)
import           MockTypes (KeyHash, LEDGER, POOL, PState, PoolParams, StakePools)
import           PParams (_eMax)
import           Slot (Epoch (..), epochFromSlot)
import           STS.Ledger (LedgerEnv (ledgerSlot))
import           STS.Pool (PoolEnv (..))
import           TxData (pattern KeyHashObj, pattern RegPool, pattern RegPool, pattern RetirePool,
                     pattern StakePools, body, certs, poolPubKey)


import           Ledger.Core (dom, (∈), (∉))
import           Test.Utils (assertAll)

-------------------------------
-- helper accessor functions --
-------------------------------

getRetiring :: PState -> Map KeyHash Epoch
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
        $ trace @POOL traceLen `ofLengthAtLeast` 1

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
  t <- forAll (trace @POOL traceLen `ofLengthAtLeast` 1)
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
                                      cepoch          = epochFromSlot s
                                      Epoch ce        = cepoch
                                      Epoch emax'     = _eMax pp in
                                    (  cepoch < e
                                    && e < Epoch (ce + emax')) ==>
                                    (  certWit ∈ dom stp
                                    && certWit ∈ dom stp'
                                    && Maybe.maybe False ((== e) . epochFromSlot) (stp' !? certWit))
            _                  -> False
        registeredPoolRetired _ _ _ = True

-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded
  :: MonadTest m
  => Environment LEDGER
  -> [SourceSignalTarget POOL]
  -> m ()
registeredPoolIsAdded env ssts =
  assertAll addedRegPool ssts

 where

  addedRegPool :: SourceSignalTarget POOL
               -> Bool
  addedRegPool sst =
    case signal sst of
      RegPool poolParams -> check poolParams
      _ -> True
   where
    check :: PoolParams -> Bool
    check poolParams =
      let hk = poolParams ^. poolPubKey
          pSt = target sst
          -- PoolParams are registered in pParams map
       in M.lookup hk (pSt ^. pParams) == Just poolParams
          -- Hashkey is registered in stPools map
       && M.lookup hk (pSt ^. stPools . to (\(StakePools x) -> x))
            == Just (ledgerSlot env)
          -- Hashkey is registered in cCounters map
       && hk ∈ M.keys (pSt ^. cCounters)

-- | Transform LEDGER `SourceSignalTargets`s to POOL ones.
ledgerToPoolSsts
  :: SourceSignalTarget LEDGER
  -> [SourceSignalTarget POOL]
ledgerToPoolSsts (SourceSignalTarget (_, DPState _ p) (_, DPState _ p') tx) =
  [SourceSignalTarget p p' cert | cert <- toList (tx ^. body . certs)]
