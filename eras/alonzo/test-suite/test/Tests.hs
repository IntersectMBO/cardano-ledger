{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Canonical as Canonical
import qualified Test.Cardano.Ledger.Alonzo.Serialisation.Tripping as Tripping
import qualified Test.Cardano.Ledger.Alonzo.Translation as Translation
import qualified Test.Cardano.Ledger.Alonzo.TxInfo as TxInfo
import qualified Test.Cardano.Ledger.Shelley.PropertyTests as Shelley
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import qualified Test.Cardano.Ledger.Shelley.Rules.IncrementalStake as IncrementalStake
import Test.Tasty

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
    [ AdaPreservation.tests @AlonzoEra @(AlonzoLEDGER AlonzoEra) 50
    , Tripping.tests
    , Translation.tests
    , Canonical.tests
    , Golden.tests
    , TxInfo.tests
    ]

nightlyTests :: TestTree
nightlyTests =
  testGroup
    "Alonzo tests - nightly"
    $ Shelley.commonTests @AlonzoEra @(AlonzoLEDGER AlonzoEra)
      ++ [ IncrementalStake.incrStakeComparisonTest (Proxy :: Proxy AlonzoEra)
         , ChainTrace.tests
         ]
