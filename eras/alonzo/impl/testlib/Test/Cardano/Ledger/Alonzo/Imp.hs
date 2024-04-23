{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Alonzo.ImpTest (MaryEraImp, withImpState)
import Test.Cardano.Ledger.Common (Spec, describe)
import qualified Test.Cardano.Ledger.Mary.Imp as MaryImp

spec ::
  forall era.
  ( MaryEraImp era
  , AlonzoEraTx era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  MaryImp.spec @era
  describe "AlonzoImpSpec" . withImpState @era $ do
    Utxos.spec @era
