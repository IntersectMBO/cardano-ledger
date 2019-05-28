{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Test.Tasty (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor(Auto))

import Ledger.Delegation.Examples (deleg)
import Ledger.Delegation.Properties (dcertsAreTriggered, rejectDupSchedDelegs)
import Ledger.Pvbump.Properties (emptyPVUpdate, beginningsNoUpdate, lastProposal)
import qualified Ledger.Delegation.Properties as DELEG
import Ledger.UTxO.Properties (moneyIsConstant)
import qualified Ledger.UTxO.Properties as UTxO

main :: IO ()
main = defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Ledger"
    [ testGroup "Delegation Examples" deleg
    , testGroup
      "Delegation Properties"
      [ testProperty "Certificates are triggered"           dcertsAreTriggered
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
      "UTxO Properties"
      [ testProperty "Money is constant" moneyIsConstant
      , testProperty "Traces are classified" UTxO.tracesAreClassified
      ]
    ]
