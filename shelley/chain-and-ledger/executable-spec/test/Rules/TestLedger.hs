{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestLedger
  ( rewardZeroAfterReg
  , credentialRemovedAfterDereg
  , consumedEqualsProduced
  , registeredPoolIsAdded
  , poolIsMarkedForRetirement
  , pStateIsInternallyConsistent
  )
where

import           Data.Foldable (toList)
import           Data.Word (Word64)
import           Lens.Micro ((^.))

import           Hedgehog (Property, forAll, property, withTests)
import qualified Test.QuickCheck as QC

import           Control.State.Transition.Generator (ofLengthAtLeast, trace,
                     traceOfLengthWithInitState)
import           Control.State.Transition.Trace (SourceSignalTarget (..), source,
                     sourceSignalTargets, target, traceEnv)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import           Generator.Core (mkGenesisLedgerState)
import qualified Generator.Core.QuickCheck as GQ
import           Generator.LedgerTrace ()
import           Generator.LedgerTrace.QuickCheck ()

import           Coin (pattern Coin)
import           LedgerState (pattern DPState, pattern DState, pattern UTxOState, _deposited,
                     _dstate, _fees, _rewards, _utxo)
import           MockTypes (DELEG, LEDGER, POOL)
import qualified Rules.TestDeleg as TestDeleg
import qualified Rules.TestPool as TestPool
import           TxData (body, certs)
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
    TestPool.registeredPoolIsAdded
      (tr ^. traceEnv)
      (concatMap ledgerToPoolSsts (sourceSignalTargets tr))


-- | Check that a `RetirePool` certificate properly removes a stake pool.
poolIsMarkedForRetirement :: QC.Property
poolIsMarkedForRetirement = do
  QC.withMaxSuccess (fromIntegral numberOfTests) . QC.property $ do
    TQC.forAllTraceFromInitState traceLen traceLen (Just GQ.mkGenesisLedgerState) $ \tr ->
      let sst = concatMap ledgerToPoolSsts (sourceSignalTargets tr)
        in TestPool.poolIsMarkedForRetirement sst

pStateIsInternallyConsistent :: Property
pStateIsInternallyConsistent = do
  withTests (fromIntegral numberOfTests) . property $ do
    tr <- forAll
          $ traceOfLengthWithInitState @LEDGER
                                     (fromIntegral traceLen)
                                     mkGenesisLedgerState
            `ofLengthAtLeast` 1
    TestPool.pStateIsInternallyConsistent
      (concatMap ledgerToPoolSsts (sourceSignalTargets tr))


-- | Transform LEDGER `sourceSignalTargets`s to DELEG ones.
ledgerToDelegSsts
  :: SourceSignalTarget LEDGER
  -> [SourceSignalTarget DELEG]
ledgerToDelegSsts (SourceSignalTarget (_, DPState d _) (_, DPState d' _) tx) =
  [SourceSignalTarget d d' cert | cert <- toList (tx ^. body . certs)]


-- | Transform LEDGER `SourceSignalTargets`s to POOL ones.
ledgerToPoolSsts
  :: SourceSignalTarget LEDGER
  -> [SourceSignalTarget POOL]
ledgerToPoolSsts (SourceSignalTarget (_, DPState _ p) (_, DPState _ p') tx) =
  [SourceSignalTarget p p' cert | cert <- toList (tx ^. body . certs)]
