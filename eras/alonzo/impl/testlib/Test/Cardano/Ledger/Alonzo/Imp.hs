{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut, AlonzoEraTxWits)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Alonzo.ImpTest (ShelleyEraImp, withImpState)
import Test.Cardano.Ledger.Common (Spec, describe)

spec ::
  forall era.
  ( ShelleyEraImp era
  , AlonzoEraTxWits era
  , AlonzoEraTxOut era
  ) =>
  Spec
spec =
  describe "AlonzoImpTest" . withImpState @era $ do
    Utxos.spec @era
