{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Imp (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Allegra.Imp.UtxowSpec as UtxowSpec
import Test.Cardano.Ledger.Common
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  ShelleyImp.spec @era
  describe "AllegraImpSpec" . withImpState @era $ do
    UtxowSpec.spec
