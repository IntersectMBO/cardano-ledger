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
import qualified Test.Cardano.Ledger.Shelley.Imp.DelegSpec as DELEG
import qualified Test.Cardano.Ledger.Shelley.Imp.EpochSpec as EPOCH
import qualified Test.Cardano.Ledger.Shelley.Imp.LedgerSpec as LEDGER
import qualified Test.Cardano.Ledger.Shelley.Imp.PoolSpec as POOL
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxoSpec as UTXO
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxowSpec as UTXOW
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
    DELEG.spec
    EPOCH.spec
    LEDGER.spec
    POOL.spec
    UTXOW.spec
    UTXO.spec
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
    DELEG.shelleyEraSpecificSpec
