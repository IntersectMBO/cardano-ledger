{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp.EpochSpec as Epoch
import qualified Test.Cardano.Ledger.Shelley.Imp.LedgerSpec as Ledger
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Shelley.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Shelley.ImpTest (LedgerSpec, ShelleyEraImp)
import qualified Test.Cardano.Ledger.Shelley.UnitTests.InstantStakeTest as Instant

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  describe "ShelleyImpSpec" $ withImpInit @(LedgerSpec era) $ do
    Ledger.spec
    Epoch.spec
    Utxow.spec
    Utxo.spec
  describe "ShelleyPureTests" $ do
    Instant.spec @era
