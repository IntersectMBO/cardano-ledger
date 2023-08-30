{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import Data.Proxy (Proxy (..))
import qualified Test.Cardano.Ledger.Babbage.TxInfo as Babbage (txInfoTests)
import Test.Cardano.Ledger.Conway.GovSnapshot (govSnapshotProps)
import qualified Test.Cardano.Ledger.Conway.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Conway.Serialisation.Roundtrip as Roundtrip
import qualified Test.Cardano.Ledger.Conway.TxInfo as Conway (txInfoTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain defaultTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Conway tests"
    [ Roundtrip.allprops @Conway
    , CDDL.tests 5
    , Babbage.txInfoTests (Proxy @Conway)
    , Conway.txInfoTests (Proxy @Conway)
    , govSnapshotProps
    ]
