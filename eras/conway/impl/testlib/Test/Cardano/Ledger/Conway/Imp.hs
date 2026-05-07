{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Imp (spec, conwayEraSpecificSpec) where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Rules (
  ConwayEpochEvent,
  ConwayHardForkEvent,
  ConwayNewEpochEvent,
 )
import Cardano.Ledger.Shelley.Rules (RupdEvent)
import Control.State.Transition.Extended
import Data.Proxy
import qualified Test.Cardano.Ledger.Babbage.Imp as Babbage
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
  ( ConwayEraImp era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Babbage.spec era
  describe "ConwayEra Onwards" $ withImpInitEachEraVersion era $ do
    Bbody.spec
    Deleg.spec
    Enact.spec
    Epoch.spec
    Gov.spec
    GovCert.spec
    Ledger.spec
    HardFork.spec
    Ratify.spec
    Utxo.spec
    Utxos.spec
    Utxow.spec

conwayEraSpecificSpec :: Spec
conwayEraSpecificSpec = do
  describe "ConwayEra Specific" $ withImpInitEachEraVersion (Proxy @ConwayEra) $ do
    Certs.spec
    Utxo.conwayEraSpecificSpec
