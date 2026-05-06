{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import Cardano.Ledger.Shelley.Rules
import qualified Test.Cardano.Ledger.Alonzo.Imp.BbodySpec as Bbody
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp as MaryImp
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

spec ::
  ( AlonzoEraImp era
  , EraSpecificSpec era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  MaryImp.spec era
  describe "AlonzoImpSpec" . withImpInitEachEraVersion era $ do
    Bbody.spec
    Utxo.spec
    Utxos.spec
    Utxow.spec

alonzoEraSpecificSpec ::
  (AlonzoEraImp era, ShelleyEraTxCert era) =>
  SpecWith (ImpInit (LedgerSpec era))
alonzoEraSpecificSpec = do
  describe "From AlonzoEra" $ do
    Utxow.alonzoEraSpecificSpec

instance EraSpecificSpec AlonzoEra where
  eraSpecificSpec =
    ShelleyImp.shelleyEraSpecificSpec >> alonzoEraSpecificSpec
