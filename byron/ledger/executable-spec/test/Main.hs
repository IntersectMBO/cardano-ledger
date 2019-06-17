{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty                             (TestTree, defaultMain,
                                                         localOption, testGroup)
import           Test.Tasty.Hedgehog                    (testProperty)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

import           Ledger.Delegation.Examples             (deleg)
import           Ledger.Delegation.Properties           (dcertsAreTriggered,
                                                         rejectDupSchedDelegs)
import qualified Ledger.Delegation.Properties           as DELEG
import           Ledger.HasTypeReps.Properties          (testTxHasTypeReps)
import           Ledger.Pvbump.Properties               (beginningsNoUpdate,
                                                         emptyPVUpdate,
                                                         lastProposal)
import qualified Ledger.Update.Properties               as UPDATE
import           Ledger.UTxO.Properties                 (moneyIsConstant)
import qualified Ledger.UTxO.Properties                 as UTxO

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Ledger"
    [ testGroup "Delegation Examples" deleg
    , testGroup
      "Delegation properties"
      [ testProperty "Certificates are triggered"           dcertsAreTriggered
      , testProperty "DBLOCK Traces are classified"         DELEG.dblockTracesAreClassified
      , testProperty "relevant DBLOCK traces generated"     DELEG.relevantCasesAreCovered
      , testProperty "Duplicated certificates are rejected" rejectDupSchedDelegs
      , testProperty "Traces are classified"                DELEG.tracesAreClassified
      ]
    , testGroup
      "PVBUMP properties"
      [ testProperty "Same state for no updates"         emptyPVUpdate
      , testProperty "Same state for early on in chain"  beginningsNoUpdate
      , testProperty "State determined by last proposal" lastProposal
      ]
    , testGroup
      "UTxO properties"
      [ testProperty "Money is constant" moneyIsConstant
      , testProperty "Traces are classified" UTxO.tracesAreClassified
      ]
    , testTxHasTypeReps
    , testGroup
      "Update UPIREG properties"
      [ testProperty "Traces are classified" UPDATE.upiregTracesAreClassified
      , testProperty "Relevant cases are classified" UPDATE.upiregRelevantTracesAreCovered
      , testProperty "Only valid signals are generated" UPDATE.onlyValidSignalsAreGenerated
      ]
    ]
