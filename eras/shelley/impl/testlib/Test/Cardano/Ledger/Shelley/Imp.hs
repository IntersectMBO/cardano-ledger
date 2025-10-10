{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Imp (spec, shelleyEraSpecificSpec) where

import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Cardano.Ledger.Shelley.TxCert (ShelleyEraTxCert)
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
  , ShelleyEraTxCert era
  , InjectRuleFailure "LEDGER" ShelleyDelegPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyPoolPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
shelleyEraSpecificSpec = do
  describe "Shelley era specific Imp spec" $
    describe "Certificates without deposits" $
      Deleg.shelleyEraSpecificSpec

instance EraSpecificSpec ShelleyEra where
  eraSpecificSpec = shelleyEraSpecificSpec
