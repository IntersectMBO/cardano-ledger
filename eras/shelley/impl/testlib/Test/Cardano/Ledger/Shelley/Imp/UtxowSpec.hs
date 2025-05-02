{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.Imp.UtxowSpec (spec) where

import qualified Cardano.Chain.Common as Byron
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Keys (asWitness, witVKeyHash)
import Cardano.Ledger.Keys.Bootstrap
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireSignature,
 )
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Core.KeyPair (ByronKeyPair (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( ShelleyEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOW" $ do
  describe "Bootstrap Witness" $ do
    it "Valid Witnesses" $ do
      aliceBootAddr <- freshBootstapAddress
      txIn <- sendCoinTo (AddrBootstrap aliceBootAddr) mempty
      let txBody = mkBasicTxBody & inputsTxBodyL .~ [txIn]
      submitTx_ (mkBasicTx txBody)
    it "InvalidWitnessesUTXOW" $ do
      aliceBootAddr@(BootstrapAddress aliceByronAddr) <- freshBootstapAddress
      aliceByronKeyPair <- getByronKeyPair aliceBootAddr
      txIn <- sendCoinTo (AddrBootstrap aliceBootAddr) mempty
      let (aliceVKey, _) = unpackByronVKey (bkpVerificationKey aliceByronKeyPair)
          txBody = mkBasicTxBody & inputsTxBodyL .~ [txIn]

          -- below we use TxBody that was not fixed up, which means the hash for signing
          -- will be different thus resulting in a wrong signature
          aliceBadWitness =
            makeBootstrapWitness
              (extractHash (hashAnnotated txBody))
              (bkpSigningKey aliceByronKeyPair)
              (Byron.addrAttributes aliceByronAddr)

          txBad =
            mkBasicTx txBody
              & (witsTxL %~ (bootAddrTxWitsL .~ [aliceBadWitness]))
      submitFailingTx txBad [injectFailure $ InvalidWitnessesUTXOW [aliceVKey]]

  it "MissingVKeyWitnessesUTXOW" $ do
    aliceKh <- freshKeyHash @'Payment
    txIn <- sendCoinTo (mkAddr aliceKh StakeRefNull) mempty
    let tx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL <>~ [txIn]
    let isAliceWitness wit = witVKeyHash wit == asWitness aliceKh
    withPostFixup (pure . (witsTxL . addrTxWitsL %~ Set.filter (not . isAliceWitness))) $
      submitFailingTx
        tx
        [ injectFailure $
            MissingVKeyWitnessesUTXOW [asWitness aliceKh]
        ]

  it "MissingScriptWitnessesUTXOW" $ do
    requiredKh <- freshKeyHash
    let scriptHash = hashScript @era $ fromNativeScript $ RequireSignature requiredKh
    txIn <- produceScript scriptHash
    let tx = mkBasicTx $ mkBasicTxBody & inputsTxBodyL <>~ [txIn]
    submitFailingTx
      tx
      [ injectFailure $
          MissingScriptWitnessesUTXOW [scriptHash]
      ]

  it "MissingTxBodyMetadataHash" $ do
    -- We can't have the size of generated auxData change, since that would affect the
    -- hard coded fee. Therefore we fix the seed.
    impSetSeed 12345
    auxData <- arbitrary @(TxAuxData era)
    let auxDataHash = hashTxAuxData auxData
        tx = mkBasicTx $ mkBasicTxBody & feeTxBodyL .~ Coin 166249
        addAuxData fixedTx = pure (fixedTx & auxDataTxL .~ SJust auxData)
    withPostFixup addAuxData $
      submitFailingTx
        tx
        [ injectFailure $
            MissingTxBodyMetadataHash auxDataHash
        ]

  it "MissingTxMetadata" $ do
    auxData <- arbitrary @(TxAuxData era)
    let auxDataHash = hashTxAuxData auxData
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . auxDataHashTxBodyL .~ SJust auxDataHash
    submitFailingTx
      tx
      [ injectFailure $
          MissingTxMetadata auxDataHash
      ]

  it "ConflictingMetadataHash" $ do
    auxData <- arbitrary @(TxAuxData era)
    let auxDataHash = hashTxAuxData auxData
    wrongAuxDataHash <- arbitrary @TxAuxDataHash
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . auxDataHashTxBodyL .~ SJust wrongAuxDataHash
            & auxDataTxL .~ SJust auxData
    submitFailingTx
      tx
      [ injectFailure $
          ConflictingMetadataHash $
            Mismatch {mismatchSupplied = wrongAuxDataHash, mismatchExpected = auxDataHash}
      ]

  it "ExtraneousScriptWitnessesUTXOW" $ do
    requiredKh <- freshKeyHash
    let script = fromNativeScript $ RequireSignature (asWitness requiredKh)
    let scriptHash = hashScript @era script
    let tx = mkBasicTx mkBasicTxBody & witsTxL . scriptTxWitsL .~ [(scriptHash, script)]
    submitFailingTx tx $
      -- We dropped validating scripts when they are not needed, starting with Babbage
      if eraProtVerLow @era >= natVersion @6
        then
          [ injectFailure $ ExtraneousScriptWitnessesUTXOW [scriptHash]
          ]
        else
          [ injectFailure $ ScriptWitnessNotValidatingUTXOW [scriptHash]
          , injectFailure $ ExtraneousScriptWitnessesUTXOW [scriptHash]
          ]
