{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Cardano.Ledger.Babel (Babel)
import Data.Proxy (Proxy (..))
import qualified Test.Cardano.Ledger.Babbage.TxInfo as Babbage (txInfoTests)
import Test.Cardano.Ledger.Babel.RulesTests (chainExamples)
import qualified Test.Cardano.Ledger.Babel.TxInfo as Babel (txInfoTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain defaultTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Babel tests"
    [ --   Babbage.txInfoTests (Proxy @Babel)
      -- , Babel.txInfoTests (Proxy @Babel)
      -- ,
      chainExamples
    ]
