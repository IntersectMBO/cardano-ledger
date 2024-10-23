{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid (spec) where

import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
 )
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
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Control.Monad ((<=<))
import Data.Maybe.Strict (StrictMaybe (..))
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~), (<&>))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec ::
  forall era.
  ( AlonzoEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "Valid transactions" $ do
  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let
          alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang :: ScriptHash (EraCrypto era)
          alwaysSucceedsNoDatumHash = hashPlutusScript $ alwaysSucceedsNoDatum slang :: ScriptHash (EraCrypto era)
          alwaysFailsWithDatumHash = hashPlutusScript $ alwaysFailsWithDatum slang :: ScriptHash (EraCrypto era)
          alwaysFailsNoDatumHash = hashPlutusScript $ alwaysFailsNoDatum slang :: ScriptHash (EraCrypto era)

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
          account <- registerStakeCredential @era $ ScriptHashObj alwaysSucceedsNoDatumHash
          expectTxSuccess <=< submitTx $
            mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(account, mempty)]

        it "Not validating WITHDRAWAL script" $ do
          account <- registerStakeCredential @era $ ScriptHashObj alwaysFailsNoDatumHash
          expectTxSuccess <=< submitPhase2Invalid $
            mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(account, mempty)]

        it "Validating MINT script" $ do
          expectTxSuccess <=< submitTx <=< mkTokenMintingTx $ alwaysSucceedsNoDatumHash

        it "Not validating MINT script" $ do
          expectTxSuccess <=< submitPhase2Invalid <=< mkTokenMintingTx $ alwaysFailsNoDatumHash

        --  Process a transaction with a succeeding script in every place possible,
        --  and also with succeeding timelock scripts.
        it "Validating scripts everywhere" $ do
          slotNo <- use impLastTickG
          addrs <- replicateM 3 freshKeyHash
          let witAlwaysScripts = replicate 2 $ RequireAllOf mempty
              witTimelockScripts =
                zip addrs [0 ..] <&> \(a, i) ->
                  RequireAllOf [RequireSignature a, RequireTimeExpire (slotNo + 100 + i)]
          txIns <- traverse (produceScript <=< impAddNativeScript) (witAlwaysScripts <> witTimelockScripts)
          let tx =
                mkBasicTx $
                  mkBasicTxBody
                    & inputsTxBodyL .~ fromList txIns
                    & vldtTxBodyL .~ ValidityInterval SNothing (SJust $ slotNo + 1)
          expectTxSuccess <=< submitTx $ tx

  it "Acceptable supplimentary datum" $ do
    const $ pendingWith "not implemented yet"
  it "Multiple identical certificates" $ do
    const $ pendingWith "not implemented yet"
  it "Non-script output with datum" $ do
    const $ pendingWith "not implemented yet"
