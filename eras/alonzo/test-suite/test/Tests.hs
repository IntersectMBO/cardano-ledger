{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules (AlonzoLEDGER)
import Data.Proxy (Proxy (..))
import System.Environment (lookupEnv)
import qualified Test.Cardano.Ledger.Alonzo.ChainTrace as ChainTrace
import qualified Test.Cardano.Ledger.Alonzo.Golden as Golden
import qualified Test.Cardano.Ledger.Alonzo.PlutusScriptExamples as PlutusScriptExamples
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.CDDL as CDDL
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Canonical as Canonical
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import qualified Test.Cardano.Ledger.Alonzo.Translation as Translation
import qualified Test.Cardano.Ledger.Alonzo.TxInfo as TxInfo
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import qualified Test.Cardano.Ledger.Shelley.PropertyTests as Shelley
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import qualified Test.Cardano.Ledger.Shelley.Rules.IncrementalStake as IncrementalStake
import Test.Tasty

type A = AlonzoEra TestCrypto

main :: IO ()
main = do
  nightly <- lookupEnv "NIGHTLY"
  defaultMain $ case nightly of
    Nothing -> defaultTests
    Just _ -> nightlyTests

defaultTests :: TestTree
defaultTests =
  testGroup
    "Alonzo tests"
    [ AdaPreservation.tests @A @(AlonzoLEDGER A) @TestCrypto 50
    , Tripping.tests
    , Translation.tests
    , Canonical.tests
    , CDDL.tests 5
    , Golden.tests
    , PlutusScriptExamples.tests
    , TxInfo.tests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Alonzo tests - nightly"
    $ Shelley.commonTests @A @(AlonzoLEDGER A) @TestCrypto
      ++ [ CDDL.tests 50
         , IncrementalStake.incrStakeComparisonTest (Proxy @A) (Proxy @TestCrypto)
         , ChainTrace.tests
         ]
