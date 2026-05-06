{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Imp (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley.Rules
import qualified Test.Cardano.Ledger.Allegra.Imp as AllegraImp
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as Utxo
import Test.Cardano.Ledger.Mary.ImpTest
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

spec ::
  ( MaryEraImp era
  , EraSpecificSpec era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  AllegraImp.spec era
  describe "MaryImpSpec" $
    withImpInitEachEraVersion era $ do
      Utxo.spec

instance EraSpecificSpec MaryEra where
  eraSpecificSpec = ShelleyImp.shelleyEraSpecificSpec
