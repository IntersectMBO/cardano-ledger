{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (ConwayEra)
import Data.Proxy (Proxy (..))
import qualified Test.Cardano.Ledger.Conway.TxInfo as Conway (txInfoTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain defaultTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Conway tests"
    [Conway.txInfoTests (Proxy @ConwayEra)]
