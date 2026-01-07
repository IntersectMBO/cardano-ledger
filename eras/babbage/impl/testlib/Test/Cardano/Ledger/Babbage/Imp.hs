{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Imp (spec) where

import Cardano.Ledger.Babbage (BabbageEra)
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import Test.Cardano.Ledger.Alonzo.ImpTest
import qualified Test.Cardano.Ledger.Babbage.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

spec :: forall era. (BabbageEraImp era, EraSpecificSpec era) => Spec
spec = do
  AlonzoImp.spec @era
  withEachEraVersion @era $
    describe "BabbageImpSpec - era generic tests" $ do
      Utxo.spec
      Utxow.spec
      Utxos.spec @era

babbageEraSpecificSpec ::
  forall era.
  AlonzoEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = do
  describe "Babbage era specific Imp spec" $
    describe "EPOCH" Epoch.babbageEraSpecificSpec

instance EraSpecificSpec BabbageEra where
  eraSpecificSpec =
    ShelleyImp.shelleyEraSpecificSpec
      >> AlonzoImp.alonzoEraSpecificSpec
      >> babbageEraSpecificSpec
