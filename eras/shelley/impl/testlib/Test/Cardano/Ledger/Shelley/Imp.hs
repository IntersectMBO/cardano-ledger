{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Imp (spec, shelleyEraSpecificSpec) where

import Cardano.Ledger.Core (EraRule)
import Cardano.Ledger.Shelley.Rules (RupdEvent)
import Cardano.Ledger.Shelley.State (ShelleyEraAccounts)
import Control.State.Transition (Event)
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp.DelegSpec as Deleg
import qualified Test.Cardano.Ledger.Shelley.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Shelley.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Shelley.Imp.PoolSpec as Pool
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Shelley.ImpTest
import qualified Test.Cardano.Ledger.Shelley.UnitTests.InstantStakeTest as Instant

spec ::
  forall proxy era.
  ( ShelleyEraImp era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  describe "ShelleyEra Onwards" $ withImpInitEachEraVersion era $ do
    Deleg.spec
    Epoch.spec
    Ledger.spec
    Pool.spec
    Utxow.spec
    Utxo.spec
  describe "ShelleyEraPureTests" $ do
    Instant.spec @era

shelleyEraSpecificSpec ::
  ( ShelleyEraImp era
  , ShelleyEraAccounts era
  ) =>
  proxy era ->
  Spec
shelleyEraSpecificSpec era = withImpInitEachEraVersion era $ do
  describe "ShelleyEra Specific" $
    Deleg.shelleyEraSpecificSpec
