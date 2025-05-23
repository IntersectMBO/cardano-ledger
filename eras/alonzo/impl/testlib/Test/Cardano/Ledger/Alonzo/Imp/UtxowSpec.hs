{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Alonzo.Core (InjectRuleFailure)
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Common

spec ::
  forall era.
  ( AlonzoEraImp era
  , InjectRuleFailure "LEDGER" ShelleyDelegPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec
