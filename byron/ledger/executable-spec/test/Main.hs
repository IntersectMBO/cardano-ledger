{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

import           Ledger.AbstractSize.Properties (testTxHasTypeReps)
import qualified Ledger.Core.Generators.Properties as CoreGen
import           Ledger.Delegation.Examples (deleg)
import qualified Ledger.Delegation.Properties as DELEG
import           Ledger.Pvbump.Properties (beginningsNoUpdate, emptyPVUpdate, firstProposal)
import           Ledger.Relation.Properties (testRelation)
import           Ledger.Update.Examples (upiendExamples)
import qualified Ledger.Update.Properties as UPDATE
import           Ledger.UTxO.Properties (moneyIsConstant)
import qualified Ledger.UTxO.Properties as UTxO

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Ledger"
    [ testGroup "Core generators properties"
      [ testProperty "Relevant k values are generated"  CoreGen.relevantKValuesAreGenerated ]
    , testGroup "Delegation Examples" deleg
    , testGroup
      "Delegation properties"
      [ testProperty "Certificates are triggered"           DELEG.dcertsAreTriggered
      , testProperty "No certificates are replayed"         DELEG.dcertsAreNotReplayed
      , testProperty "DBLOCK Traces are classified"         DELEG.dblockTracesAreClassified
      , testProperty "Relevant DBLOCK traces covered"       DELEG.relevantCasesAreCovered
      , testProperty "Duplicated certificates are rejected" DELEG.rejectDupSchedDelegs
      , testProperty "Traces are classified"                DELEG.tracesAreClassified
      , testProperty "Only valid DBLOCK signals are generated" DELEG.onlyValidSignalsAreGenerated
      , testProperty "Invalid signals are generated when requested" DELEG.invalidSignalsAreGenerated
      ]
    , testGroup
      "PVBUMP properties"
      [ testProperty "Same state for no updates"         emptyPVUpdate
      , testProperty "Same state for early on in chain"  beginningsNoUpdate
      , testProperty "State determined by first proposal" firstProposal
      ]
    , testGroup
      "UTxO properties"
      [ testProperty "Money is constant" moneyIsConstant
      , testProperty "Relevant UTxO traces are generated" UTxO.relevantCasesAreCovered
      , testProperty "No double spending" UTxO.noDoubleSpending
      , testProperty "UTxO is outputs minus inputs" UTxO.utxoDiff
      ]
    , testTxHasTypeReps
    , testGroup "Update examples" upiendExamples
    , testGroup
      "Update properties"
      [ testProperty "UPIREG traces are classified" UPDATE.upiregTracesAreClassified
      , testProperty "UBLOCK traces are classified" UPDATE.ublockTraceLengthsAreClassified
      , testProperty "Relevant UPIREG traces are covered" UPDATE.upiregRelevantTracesAreCovered
      , testProperty "Only valid UPIREG signals are generated" UPDATE.onlyValidSignalsAreGenerated
      , testProperty "Only valid UBLOCK signals are generated" UPDATE.ublockOnlyValidSignalsAreGenerated
      , testProperty "Relevant UBLOCK traces are covered" UPDATE.ublockRelevantTracesAreCovered
      , testProperty "Invalid registrations are generated when requested" UPDATE.invalidRegistrationsAreGenerated
      , testProperty "Invalid signals are generated when requested" UPDATE.invalidSignalsAreGenerated
      ]
    -- TODO move this out of here (these are not properties of the transition
    -- systems) and also move the Relation class and instances out of Ledger.Core
    , testRelation
    ]
