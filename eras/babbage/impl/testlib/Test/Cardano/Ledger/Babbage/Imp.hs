{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Babbage.Imp (
  spec,
  Alonzo.shelleyToBabbageSpec,
  Alonzo.alonzoToConwaySpec,
  babbageOnlySpec,
) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.State
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Event)
import qualified Test.Cardano.Ledger.Alonzo.Imp as Alonzo
import Test.Cardano.Ledger.Alonzo.ImpTest
import qualified Test.Cardano.Ledger.Babbage.Imp.PoolSpec as POOL
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxoSpec as UTXO
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxosSpec as UTXOS
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec as UTXOW
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common

spec ::
  ( BabbageEraImp era
  , Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Alonzo.spec era
  describe "BabbageEra Onwards" $ withImpInitEachEraVersion era $ do
    UTXO.spec
    UTXOW.spec
    UTXOS.spec

babbageOnlySpec ::
  ( BabbageEraImp era
  , ShelleyEraAccounts era
  , Event (EraRule "NEWEPOCH" era) ~ Shelley.ShelleyNewEpochEvent era
  ) =>
  proxy era ->
  Spec
babbageOnlySpec era = do
  describe "BabbageEra Specific" $ withImpInitEachEraVersion era $ do
    POOL.babbageEraSpecificSpec
