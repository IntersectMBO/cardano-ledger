{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (lookupEnv)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Constrained.Conway.LedgerTypes.Tests as LedgerTypes
import qualified Test.Cardano.Ledger.Constrained.Conway.MiniTrace as MiniTrace
import qualified Test.Cardano.Ledger.Examples.AlonzoAPI as AlonzoAPI (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoBBODY as AlonzoBBODY (tests)
import qualified Test.Cardano.Ledger.Examples.AlonzoCollectInputs as AlonzoCollectInputs (tests)
import Test.Cardano.Ledger.Examples.BabbageFeatures (babbageFeatures)
import Test.Cardano.Ledger.Generic.AggPropTests (aggTests, depositTests)
import Test.Cardano.Ledger.Generic.GenState (defaultGenSize)
import Test.Cardano.Ledger.Generic.Properties (genericProperties)
import qualified Test.Cardano.Ledger.NoThunks as NoThunks
import qualified Test.Cardano.Ledger.STS as ConstraintSTS
import Test.Cardano.Ledger.Tickf (calcPoolDistOldEqualsNew)

main :: IO ()
main = do
  nightly <- lookupEnv "NIGHTLY"
  ledgerTestMain $ do
    MiniTrace.spec
    case nightly of
      Nothing ->
        describe "cardano-core" defaultTests
      Just _ -> do
        LedgerTypes.spec
        describe "cardano-core - nightly" nightlyTests

defaultTests :: Spec
defaultTests = do
  depositTests
  calcPoolDistOldEqualsNew
  describe "STS Tests" $ do
    babbageFeatures
    AlonzoBBODY.tests
    AlonzoAPI.tests
    AlonzoCollectInputs.tests
  genericProperties defaultGenSize
  aggTests
  ConstraintSTS.tests_STS

nightlyTests :: Spec
nightlyTests = do
  defaultTests
  NoThunks.test
