{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Imp (
  spec,
  Shelley.shelleyToBabbageSpec,
  alonzoToConwaySpec,
) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import qualified Test.Cardano.Ledger.Alonzo.Imp.BbodySpec as BBODY
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec as UTXO
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as UTXOS
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec as UTXOW
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp as Mary
import qualified Test.Cardano.Ledger.Shelley.Imp as Shelley

spec ::
  ( AlonzoEraImp era
  , Shelley.Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Mary.spec era
  describe "AlonzoEra Onwards" $ withImpInitEachEraVersion era $ do
    BBODY.spec
    UTXO.spec
    UTXOS.spec
    UTXOW.spec

alonzoToConwaySpec ::
  (AlonzoEraImp era, ShelleyEraTxCert era) =>
  proxy era ->
  Spec
alonzoToConwaySpec era = withImpInitEachEraVersion era $ do
  describe "AlonzoEra Specific" $ do
    UTXOW.alonzoToConwaySpec
