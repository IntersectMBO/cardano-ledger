{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Shelley.Spec.Ledger.ShowTests (showTests) where

import           Test.Shelley.Spec.Ledger.MetaData.ShowSpec (prop_MetaData_Show)
import           Test.Tasty                                 (TestTree, testGroup)

import qualified Test.Tasty.Hedgehog as TH

showTests :: TestTree
showTests = testGroup "Show tests"
  [ TH.testProperty "Show MetaData" Test.Shelley.Spec.Ledger.MetaData.ShowSpec.prop_MetaData_Show
  ]
