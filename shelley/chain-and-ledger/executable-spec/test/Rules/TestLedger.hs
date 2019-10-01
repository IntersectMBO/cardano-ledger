{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestLedger
  ( rewardZeroAfterReg
  , consumedEqualsProduced
  )
where

import           Data.Foldable (toList)
import           Data.Word (Word64)

import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, trace,
                     traceOfLengthWithInitState)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     source, sourceSignalTargets, target)
import           Generator.Core (mkGenesisLedgerState)
import           Generator.LedgerTrace ()

import           Coin (pattern Coin)
import           LedgerState (pattern DPState, pattern DState, pattern UTxOState, _deposited,
                     _dstate, _fees, _rewards, _utxo)
import           MockTypes (DELEG, LEDGER)
import qualified Rules.TestDeleg as TestDeleg
import           TxData (_body, _certs)
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

  TestDeleg.rewardZeroAfterReg ((concatMap toCerts . sourceSignalTargets) t)

  where toCerts
          :: SourceSignalTarget LEDGER
          -> [SourceSignalTarget DELEG]
        toCerts (SourceSignalTarget (_, DPState d _) (_, DPState d' _) tx) =
          [SourceSignalTarget d d' cert | cert <- toList . _certs . _body $ tx]


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
