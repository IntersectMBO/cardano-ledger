{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Imp where

import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Babbage.Rules
import Cardano.Ledger.Babbage.TxInfo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxInfo
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Shelley.API
import Cardano.Ledger.Shelley.Rules
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Conway.Imp as ConwayImp
import Test.Cardano.Ledger.Dijkstra.ImpTest

spec ::
  forall era.
  ( DijkstraEraImp era
  , EraSpecificSpec era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (ConwayContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , InjectRuleFailure "LEDGER" ConwayCertsPredFailure era
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
  , InjectRuleFailure "LEDGER" ConwayUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ConwayUtxowPredFailure era
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
  , ToExpr (Event (EraRule "BBODY" era))
  ) =>
  Spec
spec = ConwayImp.spec @era

instance EraSpecificSpec DijkstraEra
