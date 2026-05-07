{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Imp (spec, babbageEraSpecificSpec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.State
import Cardano.Ledger.Shelley.Rules
import qualified Test.Cardano.Ledger.Alonzo.Imp as Alonzo
import Test.Cardano.Ledger.Alonzo.ImpTest
import qualified Test.Cardano.Ledger.Babbage.Imp.PoolSpec as Pool
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common

spec ::
  ( BabbageEraImp era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Alonzo.spec era
  describe "BabbageEra Onwards" $ withImpInitEachEraVersion era $ do
    Utxo.spec
    Utxow.spec
    Utxos.spec

babbageEraSpecificSpec ::
  ( BabbageEraImp era
  , ShelleyEraAccounts era
  , Event (EraRule "NEWEPOCH" era) ~ ShelleyNewEpochEvent era
  ) =>
  proxy era ->
  Spec
babbageEraSpecificSpec era = do
  describe "BabbageEra Specific" $ withImpInitEachEraVersion era $ do
    Pool.babbageEraSpecificSpec
