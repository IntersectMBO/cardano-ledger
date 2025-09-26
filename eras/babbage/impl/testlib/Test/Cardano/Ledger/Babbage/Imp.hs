{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Imp (spec, babbageEraSpecificSpec) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import Test.Cardano.Ledger.Alonzo.ImpTest
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp.PoolSpec as ShelleyImp

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
  ( BabbageEraImp era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = do
  describe "Babbage era specific Imp spec" $
    describe "Certificates without deposits" $
      describe "UTXOW" Utxow.babbageEraSpecificSpec

instance EraSpecificSpec BabbageEra where
  eraSpecificSpec =
    ShelleyImp.shelleyEraSpecificSpec
      >> AlonzoImp.alonzoEraSpecificSpec
      >> babbageEraSpecificSpec
