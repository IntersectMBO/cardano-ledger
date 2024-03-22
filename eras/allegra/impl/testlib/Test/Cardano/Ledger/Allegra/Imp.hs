{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Allegra.Imp (spec) where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Allegra.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Common (Arbitrary, Spec, describe)
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp, withImpState)

spec ::
  forall era.
  ( ShelleyEraImp era
  , NativeScript era ~ Timelock era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , Arbitrary (TxAuxData era)
  ) =>
  Spec
spec = do
  ShelleyImp.spec @era
  describe "AllegraImpTest" . withImpState @era $ do
    Utxow.spec @era
