{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Imp (spec) where

import Cardano.Ledger.Shelley (ShelleyEra)
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
  forall era.
  ( ShelleyEraImp era
  , EraSpecificSpec era
  ) =>
  Spec
spec = do
  describe "Era specific tests" . withEachEraVersion @era $ eraSpecificSpec
  describe "ShelleyImpSpec" $ withEachEraVersion @era $ do
    describe "DELEG" Deleg.spec
    Epoch.spec
    Ledger.spec
    Pool.spec
    Utxow.spec
    Utxo.spec
  describe "ShelleyPureTests" $ do
    Instant.spec @era

instance EraSpecificSpec ShelleyEra
