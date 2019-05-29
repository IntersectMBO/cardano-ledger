{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  , testGroupDelegEx
  , testGroupDeleg
  , testGroupUtxo
  , testGroupPvbump
  , testGroupUpiec
  )
where

import Test.Tasty (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor(Auto))

import Ledger.Delegation.Examples (deleg)
import Ledger.Delegation.Properties (dcertsAreTriggered, rejectDupSchedDelegs)
import Ledger.Pvbump.Properties (emptyPVUpdate, beginningsNoUpdate, lastProposal)
import Ledger.Upiec.Properties (noProtVerChange, protVerChangeAdopt, protVerChangeSameComponents, protVerChangeEmptyComponents)
import qualified Ledger.Delegation.Properties as DELEG
import Ledger.UTxO.Properties (moneyIsConstant)
import qualified Ledger.UTxO.Properties as UTxO

testGroupDelegEx :: TestTree
testGroupDelegEx = testGroup "Delegation Examples" deleg

testGroupDeleg :: TestTree
testGroupDeleg = testGroup "Delegation Properties"
  [ testProperty "Certificates are triggered"           dcertsAreTriggered
  , testProperty "Duplicated certificates are rejected" rejectDupSchedDelegs
  , testProperty "Traces are classified"                DELEG.tracesAreClassified
  ]

testGroupUtxo :: TestTree
testGroupUtxo = testGroup "UTxO Properties"
  [ testProperty "Money is constant" moneyIsConstant
  , testProperty "Traces are classified" UTxO.tracesAreClassified
  ]

testGroupPvbump :: TestTree
testGroupPvbump =  testGroup "PVBUMP properties"
  [ testProperty "Same state for no updates"         emptyPVUpdate
  , testProperty "Same state for early on in chain"  beginningsNoUpdate
  , testProperty "State determined by last proposal" lastProposal
  ]

testGroupUpiec :: TestTree
testGroupUpiec = testGroup "UPIEC properties"
  [ testProperty "Same state for the same protocol version" noProtVerChange
  , testProperty "New protocol version adopted"   protVerChangeAdopt
  , testProperty "App. vers and registered sw upd. proposals stay the same" protVerChangeSameComponents
  , testProperty "fads, rpus, cps, vts, bvs and pws get reset" protVerChangeEmptyComponents
  ]

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Ledger"
    [ testGroupDelegEx
    , testGroupDeleg
    , testGroupUtxo
    , testGroupPvbump
    , testGroupUpiec
    ]
