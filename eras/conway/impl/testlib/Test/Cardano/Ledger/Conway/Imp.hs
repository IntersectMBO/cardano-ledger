{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.BaseTypes (Inject, ShelleyBase, natVersion)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayBbodyPredFailure,
  ConwayCertsPredFailure,
  ConwayDelegPredFailure,
  ConwayEpochEvent,
  ConwayGovCertPredFailure,
  ConwayGovPredFailure,
  ConwayLedgerEvent,
  ConwayLedgerPredFailure,
  ConwayMempoolEvent,
  ConwayNewEpochEvent,
 )
import Cardano.Ledger.Conway.TxInfo (ConwayContextError)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLedgersEnv,
  ShelleyLedgersEvent,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import Control.State.Transition.Extended
import Data.Sequence (Seq)
import Data.Typeable (Typeable)
import qualified Test.Cardano.Ledger.Babbage.Imp as BabbageImp
import Test.Cardano.Ledger.Common
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
import Test.Cardano.Ledger.Conway.ImpTest (ConwayEraImp, withImpState, withImpStateWithProtVer)

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , ConwayEraImp era
  , EraSegWits era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (ConwayContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ConwayDelegPredFailure era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  , InjectRuleFailure "LEDGER" ConwayLedgerPredFailure era
  , InjectRuleFailure "BBODY" ConwayBbodyPredFailure era
  , NFData (Event (EraRule "ENACT" era))
  , ToExpr (Event (EraRule "ENACT" era))
  , Eq (Event (EraRule "ENACT" era))
  , Typeable (Event (EraRule "ENACT" era))
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "MEMPOOL" era) ~ ConwayMempoolEvent era
  , Event (EraRule "LEDGERS" era) ~ ShelleyLedgersEvent era
  , Event (EraRule "LEDGER" era) ~ ConwayLedgerEvent era
  , BaseM (EraRule "LEDGERS" era) ~ ShelleyBase
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  , STS (EraRule "LEDGERS" era)
  ) =>
  Spec
spec = do
  BabbageImp.spec @era
  describe "ConwayImpSpec - post bootstrap (protocol version 10)" $
    withImpStateWithProtVer @era (natVersion @10) $ do
      describe "BBODY" $ Bbody.spec @era
      describe "DELEG" $ Deleg.spec @era
      describe "ENACT" $ Enact.spec @era
      describe "EPOCH" $ Epoch.spec @era
      describe "GOV" $ Gov.spec @era
      describe "GOVCERT" $ GovCert.spec @era
      describe "UTXO" $ Utxo.spec @era
      describe "UTXOS" $ Utxos.spec @era
      describe "RATIFY" $ Ratify.spec @era
      describe "LEDGER" $ Ledger.spec @era
  describe "ConwayImpSpec - bootstrap phase (protocol version 9)" $
    withImpState @era $ do
      describe "BBODY" $ Bbody.spec @era
      describe "CERTS" $ Certs.spec @era
      describe "DELEG" $ Deleg.spec @era
      describe "ENACT" $ Enact.relevantDuringBootstrapSpec @era
      describe "EPOCH" $ Epoch.relevantDuringBootstrapSpec @era
      describe "GOV" $ Gov.relevantDuringBootstrapSpec @era
      describe "GOVCERT" $ GovCert.relevantDuringBootstrapSpec @era
      describe "UTXO" $ Utxo.spec @era
      describe "UTXOS" $ Utxos.relevantDuringBootstrapSpec @era
      describe "RATIFY" $ Ratify.relevantDuringBootstrapSpec @era
      describe "LEDGER" $ Ledger.spec @era
