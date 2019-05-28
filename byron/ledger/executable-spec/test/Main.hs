module Main
  ( main
  )
where

import System.Environment (withArgs)

import Test.Tasty (TestTree, defaultMain, testGroup, localOption)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Ingredients.ConsoleReporter (UseColor(Auto))
import Ledger.Delegation.Examples (deleg)
import Ledger.Delegation.Properties (dcertsAreTriggered, rejectDupSchedDelegs)
import Ledger.Pvbump.Properties (emptyPVUpdate, beginningsNoUpdate, lastProposal)


main :: IO ()
main = withArgs [] $ defaultMain tests
 where
  tests :: TestTree
  tests = localOption Auto $ testGroup
    "Ledger"
    [ testGroup "Ledger" [testGroup "Delegation Examples" deleg]
    , testGroup
      "Properties"
      [ testProperty "Activation"                      dcertsAreTriggered
      , testProperty "One delegation per-slot per-key" rejectDupSchedDelegs
      ]
    , testGroup
      "PVBUMP properties"
      [ testProperty "Same state for no updates"         emptyPVUpdate
      , testProperty "Same state for early on in chain"  beginningsNoUpdate
      , testProperty "State determined by last proposal" lastProposal
      ]
    ]
