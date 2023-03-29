{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.ShelleyDiffTests where

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState
import Test.Cardano.Data (
  propExtend,
  propZero,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Arbitrary ()

-- ==========================================================

-- We will have to tie these tests into the tests in
-- cardano-ledger/eras/shelley/testsuite/test/Tests.hs
-- The problem is this uses Tasty and the tests here use HSpec

shelleyDiffTests :: Spec
shelleyDiffTests = describe "ILC Diff tests" $ do
  describe "Diff IncrementalStake" $ do
    propZero (arbitrary @(IncrementalStake StandardCrypto))
    propExtend (arbitrary @(IncrementalStake StandardCrypto)) arbitrary
  describe "Diff UTxOState" $ do
    propZero (arbitrary @(UTxOState (ShelleyEra StandardCrypto)))
    propExtend (arbitrary @(UTxOState (ShelleyEra StandardCrypto))) arbitrary
  describe "Diff LedgerState" $ do
    propZero (arbitrary @(LedgerState (ShelleyEra StandardCrypto)))
    propExtend (arbitrary @(LedgerState (ShelleyEra StandardCrypto))) arbitrary

-- To run theses tests in ghci, uncomment and type 'main'
main :: IO ()
main = hspec $ shelleyDiffTests
