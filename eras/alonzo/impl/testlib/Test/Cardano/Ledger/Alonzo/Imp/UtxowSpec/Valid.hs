{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid (spec) where

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure,
 )
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus (
  hashPlutusScript,
  withSLanguage,
 )
import Control.Monad ((<=<))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec ::
  forall era.
  ( AlonzoEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Valid transactions" $ do
  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let
          alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang :: ScriptHash (EraCrypto era)
          alwaysSucceedsNoDatumHash = hashPlutusScript $ alwaysSucceedsNoDatum slang :: ScriptHash (EraCrypto era)
          alwaysFailsWithDatumHash = hashPlutusScript $ alwaysFailsWithDatum slang :: ScriptHash (EraCrypto era)

        it "Validating SPEND script" $ do
          txIn <- produceScript alwaysSucceedsWithDatumHash
          expectTxSuccess <=< submitTx $
            mkBasicTx $
              mkBasicTxBody & inputsTxBodyL .~ [txIn]

        it "Not validating SPEND script" $ do
          txIn <- produceScript alwaysFailsWithDatumHash
          expectTxSuccess <=< submitPhase2Invalid $
            mkBasicTx $
              mkBasicTxBody & inputsTxBodyL .~ [txIn]

        it "Validating CERT script" $ do
          txIn <- produceScript alwaysSucceedsWithDatumHash
          let txCert = RegTxCert $ ScriptHashObj alwaysSucceedsNoDatumHash
          expectTxSuccess <=< submitTx $
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & certsTxBodyL .~ [txCert]

        it "Not validating CERT script" $ do
          txIn <- produceScript alwaysFailsWithDatumHash
          let txCert = RegTxCert $ ScriptHashObj alwaysSucceedsNoDatumHash
          expectTxSuccess <=< submitPhase2Invalid $
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & certsTxBodyL .~ [txCert]

  it "Validating WITHDRAWAL script" $ do
    const $ pendingWith "not implemented yet"
  it "Not validating WITHDRAWAL script" $ do
    const $ pendingWith "not implemented yet"
  it "Validating MINT script" $ do
    const $ pendingWith "not implemented yet"
  it "Not validating MINT script" $ do
    const $ pendingWith "not implemented yet"
  it "Validating scripts everywhere" $ do
    const $ pendingWith "not implemented yet"
  it "Acceptable supplimentary datum" $ do
    const $ pendingWith "not implemented yet"
  it "Multiple identical certificates" $ do
    const $ pendingWith "not implemented yet"
  it "Non-script output with datum" $ do
    const $ pendingWith "not implemented yet"
