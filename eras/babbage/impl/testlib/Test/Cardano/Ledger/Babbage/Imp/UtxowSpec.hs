{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec, babbageEraSpecificSpec) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure, AlonzoUtxowPredFailure)
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody, InjectRuleFailure, ShelleyEraTxCert)
import Cardano.Ledger.Babbage.Rules (BabbageUtxowPredFailure)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure)
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp, ImpInit, LedgerSpec)
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" BabbageUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec

babbageEraSpecificSpec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = do
  describe "UTXOW - certificates without deposits" $ do
    Valid.babbageEraSpecificSpec
