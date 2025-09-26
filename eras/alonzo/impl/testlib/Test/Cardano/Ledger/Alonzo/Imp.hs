{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp as MaryImp
import qualified Test.Cardano.Ledger.Shelley.Imp.PoolSpec as ShelleyImp

spec ::
  forall era.
  ( AlonzoEraImp era
  , EraSpecificSpec era
  ) =>
  Spec
spec = do
  MaryImp.spec @era
  describe "AlonzoImpSpec" . withEachEraVersion @era $ do
    Utxo.spec
    Utxos.spec
    Utxow.spec

alonzoEraSpecificSpec ::
  forall era.
  (AlonzoEraImp era, ShelleyEraTxCert era) =>
  SpecWith (ImpInit (LedgerSpec era))
alonzoEraSpecificSpec = do
  describe "Alonzo era specific Imp spec" $
    describe "Certificates without deposits" $
      Utxow.alonzoEraSpecificSpec

instance EraSpecificSpec AlonzoEra where
  eraSpecificSpec = ShelleyImp.shelleyEraSpecificSpec >> alonzoEraSpecificSpec
