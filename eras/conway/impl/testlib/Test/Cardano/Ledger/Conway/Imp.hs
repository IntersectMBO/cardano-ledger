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
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition.Extended
import Data.Proxy
import qualified Test.Cardano.Ledger.Babbage.Imp as Babbage
import qualified Test.Cardano.Ledger.Conway.Imp.BbodySpec as BBODY
import qualified Test.Cardano.Ledger.Conway.Imp.CertsSpec as CERTS
import qualified Test.Cardano.Ledger.Conway.Imp.DelegSpec as DELEG
import qualified Test.Cardano.Ledger.Conway.Imp.EnactSpec as ENACT
import qualified Test.Cardano.Ledger.Conway.Imp.EpochSpec as EPOCH
import qualified Test.Cardano.Ledger.Conway.Imp.GovCertSpec as GOVCERT
import qualified Test.Cardano.Ledger.Conway.Imp.GovSpec as GOV
import qualified Test.Cardano.Ledger.Conway.Imp.HardForkSpec as HARDFORK
import qualified Test.Cardano.Ledger.Conway.Imp.LedgerSpec as LEDGER
import qualified Test.Cardano.Ledger.Conway.Imp.RatifySpec as RATIFY
import qualified Test.Cardano.Ledger.Conway.Imp.UtxoSpec as UTXO
import qualified Test.Cardano.Ledger.Conway.Imp.UtxosSpec as UTXOS
import qualified Test.Cardano.Ledger.Conway.Imp.UtxowSpec as UTXOW
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  ( ConwayEraImp era
  , Event (EraRule "HARDFORK" era) ~ ConwayHardForkEvent era
  , Event (EraRule "EPOCH" era) ~ ConwayEpochEvent era
  , Event (EraRule "NEWEPOCH" era) ~ ConwayNewEpochEvent era
  , Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Babbage.spec era
  describe "ConwayEra Onwards" $ withImpInitEachEraVersion era $ do
    BBODY.spec
    CERTS.spec
    DELEG.spec
    ENACT.spec
    EPOCH.spec
    GOV.spec
    GOVCERT.spec
    LEDGER.spec
    HARDFORK.spec
    RATIFY.spec
    UTXO.spec
    UTXOS.spec
    UTXOW.spec

conwayEraSpecificSpec :: Spec
conwayEraSpecificSpec = do
  describe "ConwayEra Specific" $ withImpInitEachEraVersion (Proxy @ConwayEra) $ do
    UTXO.conwayEraSpecificSpec
