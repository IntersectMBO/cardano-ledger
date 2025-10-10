{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Imp (spec, shelleyEraSpecificSpec) where

import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules
import Cardano.Ledger.Shelley.State (ShelleyEraAccounts)
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp.DelegSpec as Deleg
import qualified Test.Cardano.Ledger.Shelley.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Shelley.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Shelley.Imp.PoolSpec as Pool
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Shelley.ImpTest
import qualified Test.Cardano.Ledger.Shelley.UnitTests.InstantStakeTest as Instant

spec ::
  forall era.
  ( ShelleyEraImp era
  , EraSpecificSpec era
  ) =>
  Spec
spec = do
  describe "Era specific tests" . withEachEraVersion @era $ eraSpecificSpec
  describe "ShelleyImpSpec" $ withEachEraVersion @era $ do
    describe "DELEG" Deleg.spec
    Epoch.spec
    Ledger.spec
    Pool.spec
    Utxow.spec
    Utxo.spec
  describe "ShelleyPureTests" $ do
    Instant.spec @era

shelleyEraSpecificSpec ::
  forall era.
  ( ShelleyEraImp era
  , ShelleyEraAccounts era
  , InjectRuleFailure "LEDGER" ShelleyDelegsPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
shelleyEraSpecificSpec = do
  describe "Shelley era specific Imp spec" $
    describe "DELEG" $
      Deleg.shelleyEraSpecificSpec

instance EraSpecificSpec ShelleyEra where
  eraSpecificSpec = shelleyEraSpecificSpec
