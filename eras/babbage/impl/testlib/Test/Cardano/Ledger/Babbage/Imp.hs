{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Babbage.Imp (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure (..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Alonzo.Imp as AlonzoImp
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp, withImpState)
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec as Utxow
import Test.Cardano.Ledger.Common

spec ::
  forall era.
  ( Arbitrary (TxAuxData era)
  , AlonzoEraImp era
  , BabbageEraTxOut era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  Spec
spec = do
  AlonzoImp.spec @era
  describe "BabbageImpSpec" . withImpState @era $ do
    Utxow.spec @era
