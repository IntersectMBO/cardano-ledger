{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestPool where

import           Control.Monad (when)
import           Data.Map (Map, (!?))
import qualified Data.Maybe as Maybe (maybe)

import           Hedgehog (Property, forAll, property, withTests, (===))

import           Control.State.Transition.Generator (trace)
import           Control.State.Transition.Trace (sourceSignalTargets, traceLength, _traceEnv)

import           BaseTypes ((==>))
import           Delegation.Certificates (cwitness)
import           LedgerState (_retiring, _stPools)
import           MockTypes (KeyHash, POOL, PState, StakePools)
import           PParams (_eMax)
import           Slot (Epoch (..), epochFromSlot)
import           TxData (pattern KeyHashObj, pattern RegPool, pattern RetirePool,
                     pattern StakePools)


import           Ledger.Core (dom, (∈), (∉))

-------------------------------
-- helper accessor functions --
-------------------------------

getRetiring :: PState -> Map KeyHash Epoch
getRetiring p = _retiring p

getStPools :: PState -> StakePools
getStPools p = _stPools p

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Int
numberOfTests = 300

traceLen :: Int
traceLen = 100

-------------------------
-- Properties for POOL --
-------------------------

-- | Check that a newly registered pool key is not in the retiring map.
rewardZeroAfterReg :: Property
rewardZeroAfterReg = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @POOL $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t

  when (n > 1) $
    [] === filter (not . registeredPoolNotRetiring) tr

  where registeredPoolNotRetiring (_, c@(RegPool _), p') =
          case cwitness c of
            KeyHashObj certWit -> let StakePools stp = getStPools p' in
                                      (  certWit ∈ dom stp
                                      && certWit ∉ dom (getRetiring p'))
            _                  -> False
        registeredPoolNotRetiring (_, _, _) = True

-- | Check that if a pool retirement certificate is executed in the correct
-- epoch interval, then the pool key will be added to the retiring map but stays
-- in the set of stake pools.
poolRetireInEpoch :: Property
poolRetireInEpoch = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll (trace @POOL $ fromIntegral traceLen)
  let
    n :: Integer
    n = fromIntegral $ traceLength t
    tr = sourceSignalTargets t
    (s, pp) = _traceEnv t

  when (n > 1) $
    [] === filter (not . (registeredPoolRetired s pp)) tr

  where registeredPoolRetired s pp (p, c@(RetirePool _ e), p') =
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
        registeredPoolRetired _ _ (_, _, _) = True
