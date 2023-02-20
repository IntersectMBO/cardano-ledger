{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Conway (Conway)
import qualified Test.Cardano.Ledger.Conway.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Conway.Serialisation.Roundtrip as Roundtrip
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain defaultTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Conway tests"
    [ Roundtrip.allprops @Conway
    , CDDL.tests 5
    ]
