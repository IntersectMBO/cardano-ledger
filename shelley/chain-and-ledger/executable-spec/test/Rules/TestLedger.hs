{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Rules.TestLedger
  (rewardZeroAfterReg)
where

import           Data.Foldable (toList)
import           Hedgehog (Property, forAll, property, withTests)

import           Control.State.Transition.Generator (ofLengthAtLeast, traceOfLengthWithInitState)
import           Control.State.Transition.Trace (SourceSignalTarget, pattern SourceSignalTarget,
                     sourceSignalTargets)
import           Generator.Core (mkGenesisLedgerState)
import           Generator.LedgerTrace ()
import           LedgerState (pattern DPState)
import           MockTypes (DELEG, LEDGER)
import qualified Rules.TestDeleg as TestDeleg
import           TxData (_body, _certs)

------------------------------
-- Constants for Properties --
------------------------------

numberOfTests :: Int
numberOfTests = 300

traceLen :: Int
traceLen = 100

--------------------------
-- Properties for DELEG --
--------------------------

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
