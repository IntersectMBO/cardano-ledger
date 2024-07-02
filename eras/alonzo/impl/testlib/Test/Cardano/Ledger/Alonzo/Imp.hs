{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import Test.Cardano.Ledger.Alonzo.ImpTest (
  MaryEraImp,
  withImpState,
 )
import Test.Cardano.Ledger.Common (Spec, describe)
import qualified Test.Cardano.Ledger.Mary.Imp as MaryImp

spec ::
  forall era ls.
  ( MaryEraImp ls era
  , AlonzoEraTx era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  MaryImp.spec @era
  describe "AlonzoImpSpec" . withImpState @ls @era $ do
    Utxos.spec @era
