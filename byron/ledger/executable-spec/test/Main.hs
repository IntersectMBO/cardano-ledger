{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

import           Ledger.Delegation.Examples (deleg)
import qualified Ledger.Delegation.Properties as DELEG
import           Ledger.HasTypeReps.Properties (testTxHasTypeReps)
import           Ledger.Pvbump.Properties (beginningsNoUpdate, emptyPVUpdate, lastProposal)

import qualified Ledger.Update.Properties as UPDATE

import           Ledger.Relation.Properties (testRelation)
import           Ledger.UTxO.Properties (moneyIsConstant)
import qualified Ledger.UTxO.Properties as UTxO

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Ledger"
    [ testGroup "Delegation Examples" deleg
    , testGroup
      "Delegation properties"
      [ testProperty "Certificates are triggered"           DELEG.dcertsAreTriggered
      , testProperty "No certificates are replayed"         DELEG.dcertsAreNotReplayed
      , testProperty "DBLOCK Traces are classified"         DELEG.dblockTracesAreClassified
      , testProperty "Relevant DBLOCK traces covered"       DELEG.relevantCasesAreCovered
      , testProperty "Duplicated certificates are rejected" DELEG.rejectDupSchedDelegs
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
      [ testProperty "UPIREG traces are classified" UPDATE.upiregTracesAreClassified
      , testProperty "UBLOCK traces are classified" UPDATE.ublockTraceLengthsAreClassified
      , testProperty "Relevant UPIREG traces are covered" UPDATE.upiregRelevantTracesAreCovered
      , testProperty "Only valid signals are generated" UPDATE.onlyValidSignalsAreGenerated
      , testProperty "Only valid signals are generated for UBLOCK" UPDATE.ublockOnlyValidSignalsAreGenerated
      , testProperty "Relevant UBLOCK traces are covered" UPDATE.ublockRelevantTracesAreCovered
      ]
    -- TODO move this out of here (these are not properties of the transition
    -- systems) and also move the Relation class and instances out of Ledger.Core
    , testRelation
    ]
