{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure,
  AlonzoUtxosPredFailure,
  AlonzoUtxowPredFailure,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxoSpec as Utxo
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec as Utxos
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp, LedgerSpec)
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp as MaryImp

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , AlonzoEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  ) =>
  Spec
spec = do
  MaryImp.spec @era
  describe "AlonzoImpSpec" . withImpInit @(LedgerSpec era) $ do
    Utxo.spec
    Utxos.spec
    Utxow.spec
