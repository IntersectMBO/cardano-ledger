{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Imp (spec) where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, InjectRuleFailure)
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure, BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Shelley.Rules (
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import Test.Cardano.Ledger.Alonzo.ImpTest
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. (BabbageEraImp era, EraSpecificSpec era) => Spec
spec = do
  AlonzoImp.spec @era
  withEachEraVersion @era $
    describe "BabbageImpSpec - era generic tests" $ do
      Utxo.spec
      Utxow.spec
      Utxos.spec @era

instance EraSpecificSpec BabbageEra where
  eraSpecificSpec =
    AlonzoImp.alonzoEraSpecificSpec
