{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import Cardano.Ledger.Shelley.Rules
import qualified Test.Cardano.Ledger.Alonzo.Imp.BbodySpec as Bbody
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp as Mary

spec ::
  ( AlonzoEraImp era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Mary.spec era
  describe "AlonzoEra Onwards" $ withImpInitEachEraVersion era $ do
    Bbody.spec
    Utxo.spec
    Utxos.spec
    Utxow.spec

alonzoEraSpecificSpec ::
  (AlonzoEraImp era, ShelleyEraTxCert era) =>
  proxy era ->
  Spec
alonzoEraSpecificSpec era = withImpInitEachEraVersion era $ do
  describe "AlonzoEra Specific" $ do
    Utxow.alonzoEraSpecificSpec
