{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babel.Imp (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Governance ()
import Cardano.Ledger.Babel.Rules (

 )
import Cardano.Ledger.Babel.TxInfo (BabelContextError)
import Cardano.Ledger.BaseTypes (Inject, natVersion)
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Shelley.Rules (Event, ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import Data.Functor.Identity
import Data.Typeable (Typeable)
import qualified Test.Cardano.Ledger.Babbage.Imp as BabbageImp
import qualified Test.Cardano.Ledger.Babel.Imp.EnactSpec as Enact
import qualified Test.Cardano.Ledger.Babel.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Babel.Imp.GovCertSpec as GovCert
import qualified Test.Cardano.Ledger.Babel.Imp.GovSpec as Gov
import qualified Test.Cardano.Ledger.Babel.Imp.RatifySpec as Ratify
import qualified Test.Cardano.Ledger.Babel.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Babel.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Babel.ImpTest (BabelEraImp, withImpState, withImpStateWithProtVer)
import Test.Cardano.Ledger.Common

spec ::
  forall era.
  ( BabelEraImp era
  , GovState era ~ ConwayGovState era
  , PParamsHKD Identity era ~ ConwayPParams Identity era
  , InjectRuleFailure "LEDGER" ConwayGovPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  , Inject (BabelContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ConwayGovCertPredFailure era
  , NFData (Event (EraRule "ENACT" era))
  , ToExpr (Event (EraRule "ENACT" era))
  , Eq (Event (EraRule "ENACT" era))
  , Typeable (Event (EraRule "ENACT" era))
  , InjectRuleEvent "TICK" ConwayEpochEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  ) =>
  Spec
spec = do
  BabbageImp.spec @era
  describe "BabelImpSpec - post bootstrap (protocol version 10)" $
    withImpStateWithProtVer @era (natVersion @10) $ do
      Enact.spec @era
      Epoch.spec @era
      Gov.spec @era
      GovCert.spec @era
      Utxo.spec @era
      Utxos.spec @era
      Ratify.spec @era
  describe "BabelImpSpec - bootstrap phase (protocol version 9)" $
    withImpState @era $ do
      Enact.relevantDuringBootstrapSpec @era
      Epoch.relevantDuringBootstrapSpec @era
      Gov.relevantDuringBootstrapSpec @era
      GovCert.relevantDuringBootstrapSpec @era
      Utxo.spec @era
      Utxos.relevantDuringBootstrapSpec @era
      Ratify.relevantDuringBootstrapSpec @era
