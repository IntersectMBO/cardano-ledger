{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Enable standalone Data instances for ShortHash and MockDSIGN (in cardano-base)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rules.TestLedger
  ( rewardZeroAfterReg
  , credentialRemovedAfterDereg
  , consumedEqualsProduced
  , invalidDelegSignalsAreGenerated
  , registeredPoolIsAdded
  , pStateIsInternallyConsistent
  )
where

import           Data.Data (Data)
import           Data.Foldable (toList)
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import           Lens.Micro ((^.))

import           Hedgehog (MonadTest, Property, forAll, property, withTests)
import           Hedgehog.Internal.Property (CoverPercentage)

import           Control.State.Transition (PredicateFailure)
import           Control.State.Transition.Generator (SignalGenerator, TraceProfile (TraceProfile),
                     coverFailures, failures, invalidSignalsAreGeneratedForTrace, ofLengthAtLeast,
                     proportionOfValidSignals, trace, traceOfLengthWithInitState)
import           Control.State.Transition.Trace (SourceSignalTarget (..), source,
                     sourceSignalTargets, target, traceEnv)
import           Generator.Core (GenValidity (..), mkGenesisLedgerState, traceKeyPairs,
                     traceVRFKeyPairs)
import           Generator.LedgerTrace ()
import           Generator.Utxo (genTx)

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash.Short (ShortHash)
import           Coin (pattern Coin)
import           LedgerState (pattern DPState, pattern DState, pattern UTxOState, _deposited,
                     _dstate, _fees, _rewards, _utxo)
import           MockTypes (DELEG, LEDGER, MockCrypto, POOL)
import qualified Rules.TestDeleg as TestDeleg
import qualified Rules.TestPool as TestPool
import           STS.Deleg
                     (PredicateFailure (StakeKeyAlreadyRegisteredDELEG, StakeKeyNotRegisteredDELEG))
import           STS.Ledger (pattern DelegsFailure, PredicateFailure (..))
import           TxData (body, certs)
import           UTxO (balance)

import           Test.Utils (assertAll)

-- We need Data instances for these types in `cardano-base`
deriving instance Data ShortHash
deriving instance Data MockDSIGN
deriving instance Data MockCrypto

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
       (traceOfLengthWithInitState @LEDGER (fromIntegral traceLen) mkGenesisLedgerState
        `ofLengthAtLeast` 1)

  TestDeleg.rewardZeroAfterReg
    ((concatMap ledgerToDelegSsts . sourceSignalTargets) t)


credentialRemovedAfterDereg :: Property
credentialRemovedAfterDereg =
  withTests (fromIntegral numberOfTests) . property $ do
    tr <- fmap sourceSignalTargets
          $ forAll
          $ traceOfLengthWithInitState @LEDGER (fromIntegral traceLen) mkGenesisLedgerState
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
          $ traceOfLengthWithInitState @LEDGER (fromIntegral traceLen) mkGenesisLedgerState
            `ofLengthAtLeast` 1
    TestPool.registeredPoolIsAdded
      (tr ^. traceEnv)
      (concatMap ledgerToPoolSsts (sourceSignalTargets tr))


pStateIsInternallyConsistent :: Property
pStateIsInternallyConsistent = do
  withTests (fromIntegral numberOfTests) . property $ do
    tr <- forAll
          $ traceOfLengthWithInitState @LEDGER (fromIntegral traceLen) mkGenesisLedgerState
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


profile :: TraceProfile LEDGER
profile
  = TraceProfile
      { proportionOfValidSignals = 95
      , failures = [(5, tamperTx)]
      }
  where
    tamperTx :: SignalGenerator LEDGER
    tamperTx env st
      = genTx env st traceKeyPairs traceVRFKeyPairs GenInvalid

-- | The signal generator generates invalid signals with high probability when
-- invalid signals are requested.
invalidDelegSignalsAreGenerated :: Property
invalidDelegSignalsAreGenerated =
  withTests 100
    $ invalidSignalsAreGeneratedForTrace
        (traceOfLengthWithInitState @LEDGER (fromIntegral traceLen) mkGenesisLedgerState)
        (failures profile)
        (coverLedgerFailures 2)
        10 -- coverage percentage of LEDGER Predicate failures

coverLedgerFailures
  :: forall m a
   .  ( MonadTest m
      , HasCallStack
      , Data a
      )
  => CoverPercentage
  -- ^ Minimum percentage that each failure must occur.
  -> a
  -- ^ Structure containing the failures
  -> m ()
coverLedgerFailures coverPercentage =
  coverFailures
    coverPercentage
    ([ DelegsFailure (DelplFailure (DelegFailure StakeKeyAlreadyRegisteredDELEG))
     , DelegsFailure (DelplFailure (DelegFailure StakeKeyNotRegisteredDELEG)) ]
       :: [PredicateFailure LEDGER])
