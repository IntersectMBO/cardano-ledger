{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.DiffSpec (conwayDiffSpecs) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Data (
  propExtend,
  propZero,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()

-- ==========================================================

conwayDiffSpecs :: Spec
conwayDiffSpecs = describe "ILC Diff tests" $ do
  describe "Diff EnactState" $ do
    propZero (arbitrary @(EnactState (ConwayEra StandardCrypto)))
    propExtend (arbitrary @(EnactState (ConwayEra StandardCrypto))) arbitrary
  describe "Diff RatifyState" $ do
    propZero (arbitrary @(RatifyState (ConwayEra StandardCrypto)))
    propExtend (arbitrary @(RatifyState (ConwayEra StandardCrypto))) arbitrary
  describe "Diff GovernanceState" $ do
    propZero (arbitrary @(GovernanceState (ConwayEra StandardCrypto)))
    propExtend (arbitrary @(GovernanceState (ConwayEra StandardCrypto))) arbitrary

-- To run theses tests in ghci, uncomment and type 'main'
-- main :: IO ()
-- main = hspec $ conwayDiffSpecs
