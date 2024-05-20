{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Babbage.Imp (spec) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp)
import Test.Cardano.Ledger.Common

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , AlonzoEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  AlonzoImp.spec @era
