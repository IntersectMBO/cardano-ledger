{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp (spec, conwaySpec) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayEpochEvent,
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayHardForkEvent,
  ConwayLedgerPredFailure,
  ConwayNewEpochEvent,
  ConwayUtxoPredFailure,
 )
import Cardano.Ledger.Conway.TxInfo (ConwayContextError)
import Cardano.Ledger.Shelley.API.Mempool (ApplyTx (..))
import Cardano.Ledger.Shelley.Rules (
  ShelleyDelegPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Control.State.Transition.Extended
import Data.Typeable (Typeable)
import qualified Test.Cardano.Ledger.Babbage.Imp as BabbageImp
import qualified Test.Cardano.Ledger.Conway.Imp.BbodySpec as Bbody
import qualified Test.Cardano.Ledger.Conway.Imp.CertsSpec as Certs
import qualified Test.Cardano.Ledger.Conway.Imp.DelegSpec as Deleg
import qualified Test.Cardano.Ledger.Conway.Imp.EnactSpec as Enact
import qualified Test.Cardano.Ledger.Conway.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Conway.Imp.GovCertSpec as GovCert
import qualified Test.Cardano.Ledger.Conway.Imp.GovSpec as Gov
import qualified Test.Cardano.Ledger.Conway.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Conway.Imp.RatifySpec as Ratify
import qualified Test.Cardano.Ledger.Conway.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Conway.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Conway.ImpTest (
  ConwayEraImp,
  LedgerSpec,
  modifyImpInitProtVer,
 )
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest (ImpInit)

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , ConwayEraImp era
  , EraSegWits era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (ConwayContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyDelegPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ConwayDelegPredFailure era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayUtxoPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , ApplyTx era
  , NFData (Event (EraRule "ENACT" era))
  , ToExpr (Event (EraRule "ENACT" era))
  , Eq (Event (EraRule "ENACT" era))
  , Typeable (Event (EraRule "ENACT" era))
  ) =>
  Spec
spec = do
  BabbageImp.spec @era
  withImpInit @(LedgerSpec era) $
    forM_ (eraProtVersions @era) $ \protVer ->
      describe ("ConwayImpSpec - " <> show protVer) $
        modifyImpInitProtVer protVer $
          conwaySpec @era

conwaySpec ::
  forall era.
  ( ConwayEraImp era
  , EraSegWits era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (ConwayContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ConwayDelegPredFailure era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "LEDGER" ConwayUtxoPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , ApplyTx era
  , NFData (Event (EraRule "ENACT" era))
  , ToExpr (Event (EraRule "ENACT" era))
  , Eq (Event (EraRule "ENACT" era))
  , Typeable (Event (EraRule "ENACT" era))
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
conwaySpec = do
  describe "BBODY" Bbody.spec
  describe "CERTS" Certs.spec
  describe "DELEG" Deleg.spec
  describe "ENACT" Enact.spec
  describe "EPOCH" Epoch.spec
  describe "GOV" Gov.spec
  describe "GOVCERT" GovCert.spec
  describe "LEDGER" Ledger.spec
  describe "RATIFY" Ratify.spec
  describe "UTXO" Utxo.spec
  describe "UTXOS" Utxos.spec
