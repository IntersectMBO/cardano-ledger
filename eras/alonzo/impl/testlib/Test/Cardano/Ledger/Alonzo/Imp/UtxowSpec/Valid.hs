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
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  Data (..),
  hashData,
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireSignature,
 )
import Control.Monad ((<=<))
import GHC.Exts (fromList)
import Lens.Micro ((%~), (&), (.~))
import Lens.Micro.Mtl (use)
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples

import qualified Data.Map.Strict as Map
import qualified PlutusLedgerApi.Common as P

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
          alwaysSucceedsWithDatumHash = hashPlutusScript $ alwaysSucceedsWithDatum slang :: ScriptHash
          alwaysSucceedsNoDatumHash = hashPlutusScript $ alwaysSucceedsNoDatum slang :: ScriptHash
          alwaysFailsWithDatumHash = hashPlutusScript $ alwaysFailsWithDatum slang :: ScriptHash
          alwaysFailsNoDatumHash = hashPlutusScript $ alwaysFailsNoDatum slang :: ScriptHash

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
          account <- registerStakeCredential $ ScriptHashObj alwaysSucceedsNoDatumHash
          expectTxSuccess <=< submitTx $
            mkBasicTx $
              mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(account, mempty)]

        it "Not validating WITHDRAWAL script" $ do
          account <- registerStakeCredential $ ScriptHashObj alwaysFailsNoDatumHash
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
          expectTxSuccess <=< submitTx $ mkBasicTx txBody

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
          expectTxSuccess =<< submitTx tx

  it "Multiple identical certificates" $ do
    const $ pendingWith "not implemented yet"
  it "Non-script output with datum" $ do
    const $ pendingWith "not implemented yet"
