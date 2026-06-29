{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Data.Proxy (Proxy (..))
import System.Environment (lookupEnv)
import qualified Test.Cardano.Ledger.Alonzo.ChainTrace as ChainTrace
import qualified Test.Cardano.Ledger.Alonzo.Golden as Golden
import Test.Cardano.Ledger.Alonzo.ImpTest ()
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.PropertyTests as Shelley
import qualified Test.Cardano.Ledger.Shelley.Rules.AdaPreservation as AdaPreservation
import qualified Test.Cardano.Ledger.Shelley.Rules.IncrementalStake as IncrementalStake

main :: IO ()
main = do
  nightly <- lookupEnv "NIGHTLY"
  ledgerTestMain $ case nightly of
    Nothing -> defaultTests
    Just _ -> nightlyTests

defaultTests :: Spec
defaultTests =
  describe "Alonzo tests" $ do
    AdaPreservation.tests @AlonzoEra 50
    Golden.tests

nightlyTests :: Spec
nightlyTests =
  describe "Alonzo tests - nightly" $ do
    describe "Shelley common tests" $
      sequence_ $
        Shelley.commonTests @AlonzoEra
    IncrementalStake.incrStakeComparisonTest (Proxy :: Proxy AlonzoEra)
    ChainTrace.tests
