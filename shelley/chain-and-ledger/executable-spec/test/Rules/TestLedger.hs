{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestLedger
  ( rewardZeroAfterReg
  , credentialRemovedAfterDereg
  , consumedEqualsProduced
  , registeredPoolIsAdded
  )
where

import           Data.Foldable (toList)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Word (Word64)
import           Lens.Micro (to, (^.))

import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition (Environment)
import           Control.State.Transition.Generator (ofLengthAtLeast, trace,
                     traceOfLengthWithInitState)
import           Control.State.Transition.Trace (SourceSignalTarget (..), source,
                     sourceSignalTargets, target, traceEnv)
import           Generator.Core (mkGenesisLedgerState)
import           Generator.LedgerTrace ()

import           Coin (pattern Coin)
import           Ledger.Core ((∈))
import           LedgerState (pattern DPState, pattern DState, pattern UTxOState, cCounters,
                     pParams, pstate, stPools, _deposited, _dstate, _fees, _rewards, _utxo)
import           MockTypes (DCert, DELEG, LEDGER, PoolParams)
import qualified Rules.TestDeleg as TestDeleg
import           STS.Ledger (LedgerEnv (ledgerSlot))
import           TxData (pattern RegPool, StakePools (StakePools), body, certs, poolPubKey, _body,
                     _certs)
import           UTxO (balance)

import           Test.Utils (assertAll)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Word64
numberOfTests = 300

traceLen :: Word64
traceLen = 100

---------------------------
-- Properties for LEDGER --
---------------------------

-- | Check that a newly registered key has a reward of 0.
rewardZeroAfterReg :: Property
rewardZeroAfterReg = withTests (fromIntegral numberOfTests) . property $ do
  t <- forAll
       (traceOfLengthWithInitState @LEDGER
                                   (fromIntegral traceLen)
                                   mkGenesisLedgerState
        `ofLengthAtLeast` 1)

  TestDeleg.rewardZeroAfterReg
    ((concatMap ledgerToDelegSsts . sourceSignalTargets) t)


credentialRemovedAfterDereg :: Property
credentialRemovedAfterDereg =
  withTests (fromIntegral numberOfTests) . property $ do
    tr <- fmap sourceSignalTargets
          $ forAll
          $ traceOfLengthWithInitState @LEDGER
                                     (fromIntegral traceLen)
                                     mkGenesisLedgerState
            `ofLengthAtLeast` 1
    TestDeleg.credentialRemovedAfterDereg
      (concatMap ledgerToDelegSsts tr)


-- | Check that the value consumed by UTXO is equal to the value produced in
-- DELEGS
consumedEqualsProduced :: Property
consumedEqualsProduced = withTests (fromIntegral numberOfTests) . property $ do
  tr <- fmap sourceSignalTargets
        $ forAll
        $ trace @LEDGER traceLen `ofLengthAtLeast` 1

  assertAll consumedSameAsGained tr

  where consumedSameAsGained (SourceSignalTarget
                               { source = (UTxOState
                                           { _utxo = u
                                           , _deposited = d
                                           , _fees = fees
                                           }
                                          , DPState
                                            { _dstate = DState { _rewards = rewards }
                                            }
                                          )
                               , target = (UTxOState
                                            { _utxo = u'
                                            , _deposited = d'
                                            , _fees = fees'
                                            }
                                         , DPState
                                           { _dstate = DState { _rewards = rewards' }})}) =

          (balance u  + d  + fees  + foldl (+) (Coin 0) rewards ) ==
          (balance u' + d' + fees' + foldl (+) (Coin 0) rewards')


-- | Check that a `RegPool` certificate properly adds a stake pool.
registeredPoolIsAdded :: Property
registeredPoolIsAdded = do
  withTests (fromIntegral numberOfTests) . property $ do
    tr <- forAll
          $ traceOfLengthWithInitState @LEDGER
                                     (fromIntegral traceLen)
                                     mkGenesisLedgerState
            `ofLengthAtLeast` 1
    assertAll (addedRegPool (tr ^. traceEnv)) (sourceSignalTargets tr)

 where

  getTxRegPoolParams :: SourceSignalTarget LEDGER -> [PoolParams]
  getTxRegPoolParams sst =
    mapMaybe getPoolParams (toList (signal sst ^. body . certs))

  getPoolParams :: DCert -> Maybe PoolParams
  getPoolParams cert = case cert of
                         RegPool pps -> Just pps
                         _ -> Nothing

  addedRegPool :: Environment LEDGER
               -> SourceSignalTarget LEDGER
               -> Bool
  addedRegPool env sst = all check (getTxRegPoolParams sst)
   where
    check :: PoolParams -> Bool
    check poolParams =
      let hk = poolParams ^. poolPubKey
          pSt = snd (target sst) ^. pstate
          -- PoolParams are registered in pParams map
       in M.lookup hk (pSt ^. pParams) == Just poolParams
          -- Hashkey is registered in stPools map
       && M.lookup hk (pSt ^. stPools . to (\(StakePools x) -> x))
            == Just (ledgerSlot env)
          -- Hashkey is registered in cCounters map
       && hk ∈ M.keys (pSt ^. cCounters)


-- | Transform LEDGER `sourceSignalTargets`s to DELEG ones.
ledgerToDelegSsts
  :: SourceSignalTarget LEDGER
  -> [SourceSignalTarget DELEG]
ledgerToDelegSsts (SourceSignalTarget (_, DPState d _) (_, DPState d' _) tx) =
  [SourceSignalTarget d d' cert | cert <- toList . _certs . _body $ tx]
