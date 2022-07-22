{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Test.Byron.Spec.Ledger.AbstractSize.Properties (testTxHasTypeReps)
import qualified Test.Byron.Spec.Ledger.Core.Generators.Properties as CoreGen
import Test.Byron.Spec.Ledger.Delegation.Examples (deleg)
import qualified Test.Byron.Spec.Ledger.Delegation.Properties as DELEG
import Test.Byron.Spec.Ledger.Relation.Properties (testRelation)
import Test.Byron.Spec.Ledger.UTxO.Properties (moneyIsConstant)
import qualified Test.Byron.Spec.Ledger.UTxO.Properties as UTxO
import Test.Byron.Spec.Ledger.Update.Examples (upiendExamples)
import qualified Test.Byron.Spec.Ledger.Update.Properties as UPDATE
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor (Auto))

main :: IO ()
main = defaultMain tests
  where
    tests :: TestTree
    tests =
      localOption Auto $
        testGroup
          "Test.Byron.Spec.Ledger"
          [ testGroup
              "Core generators properties"
              [ testPropertyNamed
                  "Relevant k values are generated"
                  "relevant-vals-generated"
                  CoreGen.relevantKValuesAreGenerated
              ],
            testGroup "Delegation Examples" deleg,
            testGroup
              "Delegation properties"
              [ testPropertyNamed
                  "Certificates are triggered"
                  "certificates-triggered"
                  DELEG.dcertsAreTriggered,
                testPropertyNamed
                  "No certificates are replayed"
                  "no-cert-replay"
                  DELEG.dcertsAreNotReplayed,
                testPropertyNamed
                  "DBLOCK Traces are classified"
                  "dblock-traces-classified"
                  DELEG.dblockTracesAreClassified,
                testPropertyNamed
                  "Relevant DBLOCK traces covered"
                  "dblock-traces-covered"
                  DELEG.relevantCasesAreCovered,
                testPropertyNamed
                  "Duplicated certificates are rejected"
                  "duplicate-certs-rejected"
                  DELEG.rejectDupSchedDelegs,
                testPropertyNamed
                  "Traces are classified"
                  "traces-classified"
                  DELEG.tracesAreClassified,
                testPropertyNamed
                  "Only valid DBLOCK signals are generated"
                  "only-valid-dblock-signals"
                  DELEG.onlyValidSignalsAreGenerated,
                testPropertyNamed
                  "Invalid signals are generated when requested"
                  "invalid-signals-on-request"
                  DELEG.invalidSignalsAreGenerated
              ],
            testGroup
              "UTxO properties"
              [ testPropertyNamed
                  "Money is constant"
                  "constant-money"
                  moneyIsConstant,
                testPropertyNamed
                  "Relevant UTxO traces are generated"
                  "relevant-utxo-traces-generated"
                  UTxO.relevantCasesAreCovered,
                testPropertyNamed
                  "No double spending"
                  "no-double-spending"
                  UTxO.noDoubleSpending,
                testPropertyNamed
                  "UTxO is outputs minus inputs"
                  "utxo-outputs-inputs"
                  UTxO.utxoDiff,
                testPropertyNamed
                  "UTxO and txouts are disjoint"
                  "uxot-txouts-disjoint"
                  UTxO.utxoAndTxoutsMustBeDisjoint
              ],
            testTxHasTypeReps,
            testGroup "Update examples" upiendExamples,
            testGroup
              "Update properties"
              [ testPropertyNamed
                  "UPIREG traces are classified"
                  "upireg-traces-classified"
                  UPDATE.upiregTracesAreClassified,
                testPropertyNamed
                  "UBLOCK traces are classified"
                  "ublock-traces-classified"
                  UPDATE.ublockTraceLengthsAreClassified,
                testPropertyNamed
                  "Relevant UPIREG traces are covered"
                  "upireg-traces-covered"
                  UPDATE.upiregRelevantTracesAreCovered,
                testPropertyNamed
                  "Only valid UPIREG signals are generated"
                  "only-valid-upireg-signals"
                  UPDATE.onlyValidSignalsAreGenerated,
                testPropertyNamed
                  "Only valid UBLOCK signals are generated"
                  "only-valid-ublock-signals"
                  UPDATE.ublockOnlyValidSignalsAreGenerated,
                testPropertyNamed
                  "Relevant UBLOCK traces are covered"
                  "ublock-traces-covered"
                  UPDATE.ublockRelevantTracesAreCovered,
                testPropertyNamed
                  "Invalid registrations are generated when requested"
                  "invalid-registrations-on-request"
                  UPDATE.invalidRegistrationsAreGenerated,
                testPropertyNamed
                  "Invalid signals are generated when requested"
                  "invalid-signals-on-request"
                  UPDATE.invalidSignalsAreGenerated
              ],
            -- TODO move this out of here (these are not properties of the transition
            -- systems) and also move the Relation class and instances out of Byron.Spec.Ledger.Core
            testRelation
          ]
