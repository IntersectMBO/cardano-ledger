{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid (spec) where

import Cardano.Ledger.Allegra.Scripts (
  pattern RequireTimeExpire,
 )
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxosPredFailure,
 )
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unTxDatsL)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), inject, natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  Data (..),
  hashData,
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyDelegPredFailure (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import qualified Data.Map.Strict as Map
import GHC.Exts (fromList)
import Lens.Micro ((%~), (&), (.~))
import Lens.Micro.Mtl (use)
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Core.Utils
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

spec ::
  forall era.
  ( AlonzoEraImp era
  , ShelleyEraTxCert era
  , InjectRuleFailure "LEDGER" ShelleyDelegPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "Valid transactions" $ do
  it "Non-script output with datum" $ do
    -- Attach a datum (hash) to a non-script output and then spend it.
    -- Note that the datum cannot be supplied when spending the output,
    -- because it's considered extraneous.
    addr <- mkAddr <$> freshKeyHash @'Payment <*> pure StakeRefNull
    amount <- Coin <$> choose (2_000_000, 8_000_000)
    let
      datumHash = hashData @era $ Data (P.I 123)
      txOut = mkBasicTxOut addr (inject amount) & dataHashTxOutL .~ SJust datumHash
      tx1 = mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut]
    txIn <- txInAt 0 <$> submitTx tx1
    let
      tx2 = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    submitTx_ tx2

  forM_ (eraLanguages @era) $ \lang ->
    withSLanguage lang $ \slang ->
      describe (show lang) $ do
        let
          alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang :: ScriptHash
          alwaysSucceedsNoDatumHash = hashPlutusScript $ alwaysSucceedsNoDatum slang :: ScriptHash
          alwaysFailsWithDatumHash = hashPlutusScript $ alwaysFailsWithDatum slang :: ScriptHash
          alwaysFailsNoDatumHash = hashPlutusScript $ alwaysFailsNoDatum slang :: ScriptHash

        it "Validating SPEND script" $ do
          txIn <- produceScript alwaysSucceedsWithDatumHash
          submitTx_ $
            mkBasicTx $
              mkBasicTxBody & inputsTxBodyL .~ [txIn]

        it "Not validating SPEND script" $ do
          txIn <- produceScript alwaysFailsWithDatumHash
          submitPhase2Invalid_ $
            mkBasicTx $
              mkBasicTxBody & inputsTxBodyL .~ [txIn]

        it "Validating CERT script" $ do
          txIn <- produceScript alwaysSucceedsWithDatumHash
          let txCert = RegTxCert $ ScriptHashObj alwaysSucceedsNoDatumHash
          submitTx_ $
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & certsTxBodyL .~ [txCert]

        it "Not validating CERT script" $ do
          txIn <- produceScript alwaysFailsWithDatumHash
          let txCert = RegTxCert $ ScriptHashObj alwaysSucceedsNoDatumHash
          submitPhase2Invalid_ $
            mkBasicTx $
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & certsTxBodyL .~ [txCert]

        it "Validating WITHDRAWAL script" $ do
          account <- registerStakeCredential $ ScriptHashObj alwaysSucceedsNoDatumHash
          submitTx_ $
            mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(account, mempty)]

        it "Not validating WITHDRAWAL script" $ do
          account <- registerStakeCredential $ ScriptHashObj alwaysFailsNoDatumHash
          submitPhase2Invalid_ $
            mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(account, mempty)]

        it "Validating MINT script" $ do
          submitTx_ =<< mkTokenMintingTx alwaysSucceedsNoDatumHash

        it "Not validating MINT script" $ do
          submitPhase2Invalid_ =<< mkTokenMintingTx alwaysFailsNoDatumHash

        --  Process a transaction with a succeeding script in every place possible,
        --  and also with succeeding timelock scripts.
        it "Validating scripts everywhere" $ do
          slotNo <- use impLastTickG
          let
            timelockScriptHash i = do
              addr <- freshKeyHash
              impAddNativeScript $
                RequireAllOf [RequireSignature addr, RequireTimeExpire (slotNo + 100 + i)]
            scriptAsset scriptHash = do
              Positive amount <- arbitrary
              pure (PolicyID scriptHash, fromList [(AssetName "Test Asset", amount)])
          timelockScriptHash0 <- timelockScriptHash 0
          timelockScriptHash1 <- timelockScriptHash 1
          timelockScriptHash2 <- timelockScriptHash 2
          let
            inputScriptHashes = [alwaysSucceedsWithDatumHash, timelockScriptHash0]
            assetScriptHashes = [alwaysSucceedsNoDatumHash, timelockScriptHash1]
            rewardScriptHashes = [alwaysSucceedsNoDatumHash, timelockScriptHash2]
          txIns <- traverse produceScript inputScriptHashes
          multiAsset <- MultiAsset . fromList <$> traverse scriptAsset assetScriptHashes
          rewardAccounts <- traverse (registerStakeCredential . ScriptHashObj) rewardScriptHashes
          outputAddr <- freshKeyHash @'Payment
          let
            txOut =
              mkBasicTxOut
                (mkAddr outputAddr StakeRefNull)
                (MaryValue mempty multiAsset)
            txBody =
              mkBasicTxBody
                & inputsTxBodyL .~ fromList txIns
                & vldtTxBodyL .~ ValidityInterval SNothing (SJust $ slotNo + 1)
                & mintTxBodyL .~ multiAsset
                & withdrawalsTxBodyL .~ Withdrawals (fromList [(acct, mempty) | acct <- rewardAccounts])
                & certsTxBodyL .~ fromList (UnRegTxCert . ScriptHashObj <$> rewardScriptHashes)
                & outputsTxBodyL .~ [txOut]
          submitTx_ $ mkBasicTx txBody

        it "Acceptable supplementary datum" $ do
          inputAddr <- freshKeyHash @'Payment
          amount <- Coin <$> choose (2_000_000, 8_000_000)
          txIn <- sendCoinTo (mkAddr inputAddr StakeRefNull) amount
          let
            datum = Data (P.I 123)
            datumHash = hashData datum
            txOut =
              mkBasicTxOut
                (mkAddr alwaysSucceedsWithDatumHash StakeRefNull)
                (MaryValue amount mempty)
                & dataHashTxOutL .~ SJust datumHash
            txBody =
              mkBasicTxBody
                & inputsTxBodyL .~ [txIn]
                & outputsTxBodyL .~ [txOut]
            tx =
              mkBasicTx txBody
                & witsTxL . datsTxWitsL . unTxDatsL %~ Map.insert datumHash datum
          submitTx_ tx

        it "Multiple identical certificates" $ do
          let scriptHash = alwaysSucceedsNoDatumHash
          void . registerStakeCredential $ ScriptHashObj scriptHash
          let tx =
                mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ fromList (UnRegTxCert . ScriptHashObj <$> replicate 2 scriptHash)
          if eraProtVerLow @era < natVersion @9
            then
              -- This passes UTXOW rules but not DELEG rules; however, we care about only UTXOW rules here
              submitFailingTx
                tx
                [injectFailure $ StakeKeyNotRegisteredDELEG (ScriptHashObj scriptHash)]
            else
              -- Conway fixed the bug that was causing DELEG to fail
              submitTx_ tx
