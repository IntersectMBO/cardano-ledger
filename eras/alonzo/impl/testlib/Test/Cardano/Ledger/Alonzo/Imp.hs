{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Alonzo.ImpTest (ShelleyEraImp, withImpState)
import Test.Cardano.Ledger.Common (Spec, describe)

spec ::
  forall era.
  ( ShelleyEraImp era
  , AlonzoEraTx era
  ) =>
  Spec
spec =
  describe "AlonzoImpTest" . withImpState @era $ do
    Utxos.spec @era
