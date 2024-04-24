{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Imp (spec) where

import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Allegra.Imp as AllegraImp
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as Utxo
import Test.Cardano.Ledger.Mary.ImpTest (MaryEraImp)
import Test.Cardano.Ledger.Shelley.ImpTest (withImpState)

spec ::
  forall era.
  ( MaryEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  AllegraImp.spec @era
  describe "MaryImpSpec" $ withImpState @era $ do
    Utxo.spec @era
