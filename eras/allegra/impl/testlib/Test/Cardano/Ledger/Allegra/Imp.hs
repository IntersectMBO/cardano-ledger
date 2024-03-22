{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Imp where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Allegra.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Common (Spec, describe)
import Test.Cardano.Ledger.Shelley.ImpTest (ShelleyEraImp, withImpState)

spec ::
  forall era.
  ( ShelleyEraImp era
  , NativeScript era ~ Timelock era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec =
  describe "AllegraImpTest" . withImpState @era $ do
    Utxow.spec @era
