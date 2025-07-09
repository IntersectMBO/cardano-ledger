{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Dijkstra.Imp where

import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Babbage.Rules
import Cardano.Ledger.Babbage.TxInfo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxInfo
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Imp as ConwayImp
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( DijkstraEraImp era
  , EraPlutusTxInfo 'PlutusV2 era
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
  ConwayImp.spec @era
  withImpInit @(LedgerSpec era) $ do
    -- TODO
    pure ()
