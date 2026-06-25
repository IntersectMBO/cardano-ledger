{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Imp (spec) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent,
  ConwayHardForkEvent,
  ConwayNewEpochEvent,
 )
import Control.State.Transition.Extended
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import qualified Test.Cardano.Ledger.Babbage.Imp as BabbageImp
import qualified Test.Cardano.Ledger.Conway.Imp.BbodySpec as Bbody
import qualified Test.Cardano.Ledger.Conway.Imp.CertsSpec as Certs
import qualified Test.Cardano.Ledger.Conway.Imp.DelegSpec as Deleg
import qualified Test.Cardano.Ledger.Conway.Imp.EnactSpec as Enact
import qualified Test.Cardano.Ledger.Conway.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Conway.Imp.GovCertSpec as GovCert
import qualified Test.Cardano.Ledger.Conway.Imp.GovSpec as Gov
import qualified Test.Cardano.Ledger.Conway.Imp.HardForkSpec as HardFork
import qualified Test.Cardano.Ledger.Conway.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Conway.Imp.RatifySpec as Ratify
import qualified Test.Cardano.Ledger.Conway.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Conway.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Conway.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( ConwayEraImp era
  , EraSpecificSpec era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  Spec
spec = do
  BabbageImp.spec @era
  withEachEraVersion @era $ conwayEraGenericSpec @era

conwayEraGenericSpec ::
  forall era.
  ( ConwayEraImp era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
conwayEraGenericSpec = do
  describe "BBODY" Bbody.spec
  describe "CERTS" Certs.spec
  describe "DELEG" Deleg.spec
  describe "ENACT" Enact.spec
  describe "EPOCH" Epoch.spec
  describe "GOV" Gov.spec
  describe "GOVCERT" GovCert.spec
  describe "LEDGER" Ledger.spec
  describe "HARDFORK" HardFork.spec
  describe "RATIFY" Ratify.spec
  describe "UTXO" Utxo.spec
  describe "UTXOS" Utxos.spec
  describe "UTXOW" Utxow.spec

conwayEraSpecificSpec :: SpecWith (ImpInit (LedgerSpec ConwayEra))
conwayEraSpecificSpec = do
  describe "Conway era specific Imp spec" $ do
    describe "UTXO" Utxo.conwayEraSpecificSpec

instance EraSpecificSpec ConwayEra where
  eraSpecificSpec =
    AlonzoImp.alonzoEraSpecificSpec
      >> conwayEraSpecificSpec
