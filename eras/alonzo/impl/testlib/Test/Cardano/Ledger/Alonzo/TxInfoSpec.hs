{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Alonzo.TxInfoSpec (
  spec,
  txInfoSpec,
  txInfoSignersSpec,
  txInfoCertsSpec,
  EraPlutusTxOut (..),
  EraTranslateValidityInterval (..),
  -- ** Assertions
  expectTxIns,
  expectTxOutputs,
  expectWithdrawals,
  expectFee,
  expectMintValue,
  expectDatums,
  expectTxId,
  expectValidityRange,
  expectSignatories,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (
  ContextError,
  EraPlutusTxInfo (..),
  LedgerTxInfo (..),
  PlutusTxInfo, toPlutusTxInfoForPurpose, toPlutusTxCert,
 )
import Cardano.Ledger.Alonzo.Scripts (AsPurpose (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (TranslationLogicMissingInput, TimeTranslationPastHorizon), transMultiAsset, transWithdrawals, transTxOut, transValue, transTxWitsDatums, transTxBodyId, transValidityInterval)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.TxWits (TxDats)
import Cardano.Ledger.BaseTypes (
  ProtVer (ProtVer),
  StrictMaybe (SJust, SNothing), Inject (inject),
 )
import Data.Proxy (Proxy (..))
import Cardano.Ledger.Coin (Coin (Coin))
import qualified GHC.Exts as GHC
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import qualified PlutusLedgerApi.V3 as PV4
import qualified PlutusLedgerApi.V3.MintValue as PV3
import Cardano.Ledger.Mary.Value (MaryValue(MaryValue), MultiAsset, valueFromList)
import Cardano.Ledger.Plutus.Language (Language (..), SLanguage (..))
import Cardano.Ledger.State (UTxO (..), txinLookup)
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Functor.Identity (Identity)
import Data.Maybe (mapMaybe)
import Cardano.Ledger.Plutus.TxInfo (transAccountAddress, transCoinToLovelace, transKeyHash, transTxIn, transSafeHash)
import Data.Bifunctor (bimap)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Common
import qualified PlutusTx.AssocMap as AssocMap

type family PlutusTxOut (l :: Language) where
  PlutusTxOut 'PlutusV1 = PV1.TxOut
  PlutusTxOut 'PlutusV2 = PV2.TxOut
  PlutusTxOut 'PlutusV3 = PV3.TxOut
  PlutusTxOut 'PlutusV4 = PV3.TxOut

-- | Type class for era- and language-specific output assertion in TxInfo tests.
-- Instances for V1 are defined here; instances for V2/V3/V4 are defined in the
-- Babbage testlib (they require 'BabbageEraTxOut' and 'transTxOutV2').
class EraPlutusTxOut (l :: Language) era where
  toPlutusTxOut :: proxy l -> TxOut era -> Maybe (PlutusTxOut l)

instance (AlonzoEraTxOut era, Value era ~ MaryValue) => EraPlutusTxOut 'PlutusV1 era where
  toPlutusTxOut _ = transTxOut

-- | Type class for era-specific validity interval translation in tests.
class EraTranslateValidityInterval era where
  translateVI ::
    EpochInfo (Either Text.Text) ->
    SystemStart ->
    ValidityInterval ->
    Either (ContextError era) PV1.POSIXTimeRange

instance EraTranslateValidityInterval AlonzoEra where
  translateVI = transValidityInterval (Proxy @AlonzoEra)

spec ::
  forall era.
  ( EraTx era
  , AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , Value era ~ MaryValue
  , EraPlutusTxInfo 'PlutusV1 era
  , EraPlutusTxOut 'PlutusV1 era
  , EraTranslateValidityInterval era
  , Inject (AlonzoContextError era) (ContextError era)
  , Arbitrary (TxCert era)
  , AtMostEra "Conway" era
  ) =>
  Spec
spec = do
  describe "txInfo translation" $ do
    txInfoSpec @era SPlutusV1
    txInfoSignersSpec @era SPlutusV1
    txInfoCertsSpec @era SPlutusV1

txInfoSpec ::
  forall era l.
  ( EraTx era
  , AlonzoEraTxBody era
  , AlonzoEraTxWits era
  , Value era ~ MaryValue
  , EraPlutusTxInfo l era
  , EraPlutusTxOut l era
  , EraTranslateValidityInterval era
  , Inject (AlonzoContextError era) (ContextError era)
  , AtMostEra "Conway" era
  ) =>
  SLanguage l ->
  Spec
txInfoSpec slang = do
  describe (show slang) $ do
    prop "correctly translate tx with alonzo-era features" $ do
      inputs <- listOf1 genUtxoEntry
      outputs <- listOf1 genTxOut
      feeCoin <- arbitrary
      mintValue <- arbitrary
      wdrls <- arbitrary
      validityRange <- arbitrary
      signers <- arbitrary
      datums <- arbitrary @(TxDats era)
      let utxoSet = UTxO $ Map.fromList inputs
          txIns = Set.fromList $ fmap fst inputs
          txBody =
            mkBasicTxBody
              & inputsTxBodyL .~ txIns
              & outputsTxBodyL .~ StrictSeq.fromList outputs
              & feeTxBodyL .~ feeCoin
              & mintTxBodyL .~ mintValue
              & withdrawalsTxBodyL .~ wdrls
              & vldtTxBodyL .~ validityRange
              & reqSignerHashesTxBodyL .~ signers
          tx = mkBasicTx txBody & witsTxL . datsTxWitsL .~ datums
      pure $
        successfulTranslation @era
          slang
          utxoSet
          tx
          -- Assertions
          $ \_slang txInfo -> do
            -- Inputs with Byron addresses (or StakeRefPtr) are silently dropped in Alonzo
            expectTxIns slang utxoSet txIns txInfo

            -- Outputs with Byron addresses are also silently dropped in Alonzo.
            expectTxOutputs slang outputs txInfo

            -- Assertions on the fee in the TxInfo
            expectFee slang feeCoin txInfo

            -- Assertions on the mint value in the TxInfo
            expectMintValue slang mintValue txInfo

            -- Assertions on the withdrawals in the TxInfo
            expectWithdrawals slang wdrls txInfo

            -- Assertions on the datums in the TxInfo
            expectDatums slang (tx ^. witsTxL) txInfo

            -- Assertions on the txId in the TxInfo
            expectTxId slang txBody txInfo

            -- Assertions on the validity range in the TxInfo
            expectValidityRange @era slang validityRange txInfo

            -- Assertions on the required signatories in the TxInfo
            expectSignatories signers slang txInfo


    prop "translation fails when input not in UTxO" $ do
      -- Generate a valid transaction with inputs
      inputs <- listOf1 genUtxoEntry
      outputs <- listOf1 genTxOut
      feeCoin <- arbitrary

      -- Pick one input to exclude from the UTxO
      missingInputIdx <- choose (0, length inputs - 1)
      let (missingTxIn, _) = inputs !! missingInputIdx
          -- Create UTxO without the missing input
          utxoSet = UTxO $ Map.fromList $ filter ((/= missingTxIn) . fst) inputs
          txIns = Set.fromList $ fmap fst inputs
          txBody =
            mkBasicTxBody @era
              & inputsTxBodyL .~ txIns
              & outputsTxBodyL .~ StrictSeq.fromList outputs
              & feeTxBodyL .~ feeCoin
          tx = mkBasicTx txBody
          lti =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = ei
              , ltiSystemStart = ss
              , ltiUTxO = utxoSet
              , ltiTx = tx
              }

      let expectedError = TranslationLogicMissingInput @era missingTxIn

      pure $ case toPlutusTxInfoForPurpose slang lti (SpendingPurpose AsPurpose) of
        Left err ->
          err === inject expectedError
        Right _ ->
          counterexample "Expected translation to fail when input not in UTxO" False

    prop "translation fails for slot past horizon" $ do
      inputs <- listOf1 (genUtxoEntry @era)
      outputs <- listOf1 genTxOut
      feeCoin <- arbitrary

      -- Create an EpochInfo that always fails (simulating a horizon error)
      let timeTranslationErrMsg = Text.pack "Slot is past the forecasting horizon"
          failingEpochInfo :: EpochInfo (Either Text.Text)
          failingEpochInfo = hoistEpochInfo
            (\_ -> Left timeTranslationErrMsg)
            $ fixedEpochInfo @Identity (EpochSize 100) (mkSlotLength 1)

      -- Any slot will trigger the error with failingEpochInfo
      veryFarSlot <- SlotNo <$> arbitrary
      let validityRange = ValidityInterval (SJust veryFarSlot) SNothing
          utxoSet = UTxO $ Map.fromList inputs
          txIns = Set.fromList $ fmap fst inputs
          txBody =
            mkBasicTxBody
              & inputsTxBodyL .~ txIns
              & outputsTxBodyL .~ StrictSeq.fromList outputs
              & feeTxBodyL .~ feeCoin
              & vldtTxBodyL .~ validityRange
          tx = mkBasicTx txBody
          lti =
            LedgerTxInfo
              { ltiProtVer = ProtVer (eraProtVerLow @era) 0
              , ltiEpochInfo = failingEpochInfo
              , ltiSystemStart = ss
              , ltiUTxO = utxoSet
              , ltiTx = tx
              }

      let expectedError = TimeTranslationPastHorizon @era timeTranslationErrMsg

      pure $ case toPlutusTxInfoForPurpose slang lti (SpendingPurpose AsPurpose) of
        Left err ->
          err === inject expectedError
        Right _ ->
          counterexample "Expected translation to fail for slot past horizon" False

expectTxIns ::
  forall era l.
  ( AlonzoEraTxBody era
  , Value era ~ MaryValue
  , EraPlutusTxInfo l era
  ) =>
  SLanguage l ->
  UTxO era ->
  Set TxIn ->
  PlutusTxInfo l ->
  Expectation
expectTxIns slang utxoSet txIns txInfo =
  case slang of
    SPlutusV1 -> do
      let expectedInputs =
            mapMaybe
              (\txIn -> PV1.TxInInfo (transTxIn txIn) <$> (transTxOut @era =<< txinLookup txIn utxoSet))
              (Set.toList txIns)
      PV1.txInfoInputs txInfo `shouldBe` expectedInputs
    SPlutusV2 -> do
      expectedInputs <- traverse (expectRight . toPlutusTxInInfo slang utxoSet) (Set.toList txIns)
      PV2.txInfoInputs txInfo `shouldBe` expectedInputs
    SPlutusV3 -> do
      expectedInputs <- traverse (expectRight . toPlutusTxInInfo slang utxoSet) (Set.toList txIns)
      PV3.txInfoInputs txInfo `shouldBe` expectedInputs
    SPlutusV4 -> do
      expectedInputs <- traverse (expectRight . toPlutusTxInInfo slang utxoSet) (Set.toList txIns)
      PV4.txInfoInputs txInfo `shouldBe` expectedInputs

expectTxOutputs ::
  forall era l.
  (EraPlutusTxOut l era) =>
  SLanguage l ->
  [TxOut era] ->
  PlutusTxInfo l ->
  Expectation
expectTxOutputs slang outputs txInfo = do
  let expectedOutputs = mapMaybe (toPlutusTxOut slang) outputs
  case slang of
    SPlutusV1 -> do
      PV1.txInfoOutputs txInfo `shouldBe` expectedOutputs
    SPlutusV2 -> do
      PV2.txInfoOutputs txInfo `shouldBe` expectedOutputs
    SPlutusV3 -> do
      PV3.txInfoOutputs txInfo `shouldBe` expectedOutputs
    SPlutusV4 -> do
      PV4.txInfoOutputs txInfo `shouldBe` expectedOutputs

expectTxCerts ::
  forall era l.
  ( EraPlutusTxInfo l era
  ) =>
  ProtVer ->
  StrictSeq (TxCert era) ->
  SLanguage l ->
  PlutusTxInfo l ->
  Expectation
expectTxCerts pv certs slang txInfo = do
  expectedCerts <- traverse (expectRight . toPlutusTxCert @l @era slang pv) $ GHC.toList certs
  case slang of
    SPlutusV1 -> PV1.txInfoDCert txInfo `shouldBe` expectedCerts
    SPlutusV2 -> PV2.txInfoDCert txInfo `shouldBe` expectedCerts
    SPlutusV3 -> PV3.txInfoTxCerts txInfo `shouldBe` expectedCerts
    SPlutusV4 -> PV4.txInfoTxCerts txInfo `shouldBe` expectedCerts

expectFee ::
  SLanguage l ->
  Coin ->
  PlutusTxInfo l ->
  Expectation
expectFee slang feeCoin txInfo =
  case slang of
    SPlutusV1 ->
      AssocMap.toList (PV1.getValue $ PV1.txInfoFee txInfo) `shouldBe`
        AssocMap.toList (PV1.getValue $ transValue $ valueFromList feeCoin [])
    SPlutusV2 ->
      AssocMap.toList (PV1.getValue $ PV2.txInfoFee txInfo) `shouldBe`
        AssocMap.toList (PV1.getValue $ transValue $ valueFromList feeCoin [])
    SPlutusV3 -> PV3.txInfoFee txInfo `shouldBe` transCoinToLovelace feeCoin
    SPlutusV4 -> PV4.txInfoFee txInfo `shouldBe` transCoinToLovelace feeCoin

expectMintValue ::
  SLanguage l ->
  MultiAsset ->
  PlutusTxInfo l ->
  Expectation
expectMintValue slang mintValue txInfo =
  case slang of
    SPlutusV1 ->
      AssocMap.toList (PV1.getValue $ PV1.txInfoMint txInfo) `shouldBe`
        -- PV1 adds an empty ada coin value in the Map, hence why we
        -- need to create a 'MaryValue' with 0 Ada explicitly.
        AssocMap.toList (PV1.getValue $ transValue $ MaryValue (Coin 0) mintValue)
    SPlutusV2 ->
        -- PV2 adds an empty ada coin value in the Map, hence why we
        -- need to create a 'MaryValue' with 0 Ada explicitly.
      AssocMap.toList (PV1.getValue $ PV2.txInfoMint txInfo) `shouldBe`
        AssocMap.toList (PV1.getValue $ transValue $ MaryValue (Coin 0) mintValue)
    SPlutusV3 ->
      PV3.txInfoMint txInfo `shouldBe`
        PV3.UnsafeMintValue (PV1.getValue (transMultiAsset mintValue))
    SPlutusV4 ->
      PV4.txInfoMint txInfo `shouldBe`
        PV3.UnsafeMintValue (PV1.getValue (transMultiAsset mintValue))

expectWithdrawals ::
  SLanguage l ->
  Withdrawals ->
  PlutusTxInfo l ->
  Expectation
expectWithdrawals slang wdrls txInfo =
  case slang of
    SPlutusV1 -> PV1.txInfoWdrl txInfo `shouldBe` Map.toList (transWithdrawals wdrls)
    SPlutusV2 -> PV2.txInfoWdrl txInfo `shouldBe` AssocMap.unsafeFromList (Map.toList (transWithdrawals wdrls))
    SPlutusV3 ->
      AssocMap.toList (PV3.txInfoWdrl txInfo) `shouldBe`
        fmap (bimap transAccountAddress transCoinToLovelace) (Map.toList (unWithdrawals wdrls))
    SPlutusV4 ->
      AssocMap.toList (PV4.txInfoWdrl txInfo) `shouldBe`
        fmap (bimap transAccountAddress transCoinToLovelace) (Map.toList (unWithdrawals wdrls))

expectDatums ::
  forall era l.
  AlonzoEraTxWits era =>
  SLanguage l ->
  TxWits era ->
  PlutusTxInfo l ->
  Expectation
expectDatums slang wits txInfo =
  let datums = transTxWitsDatums wits
   in case slang of
        SPlutusV1 -> PV1.txInfoData txInfo `shouldBe` datums
        SPlutusV2 -> PV2.txInfoData txInfo `shouldBe` AssocMap.unsafeFromList datums
        SPlutusV3 -> PV3.txInfoData txInfo `shouldBe` AssocMap.unsafeFromList datums
        SPlutusV4 -> PV4.txInfoData txInfo `shouldBe` AssocMap.unsafeFromList datums

expectTxId ::
  forall era lvl l.
  ( EraTxBody era
  ) =>
  SLanguage l ->
  TxBody lvl era ->
  PlutusTxInfo l ->
  Expectation
expectTxId slang txBody txInfo =
  case slang of
    SPlutusV1 -> PV1.txInfoId txInfo `shouldBe` transTxBodyId txBody
    SPlutusV2 -> PV2.txInfoId txInfo `shouldBe` transTxBodyId txBody
    SPlutusV3 -> PV3.txInfoId txInfo `shouldBe` PV3.TxId (transSafeHash (hashAnnotated @_ @EraIndependentTxBody txBody))
    SPlutusV4 -> PV4.txInfoId txInfo `shouldBe` PV3.TxId (transSafeHash (hashAnnotated @_ @EraIndependentTxBody txBody))

expectSignatories ::
  forall l.
  Set (KeyHash Guard) ->
  SLanguage l ->
  PlutusTxInfo l ->
  Expectation
expectSignatories signers slang txInfo = do
  case slang of
    SPlutusV1 -> PV1.txInfoSignatories txInfo `shouldBe` fmap transKeyHash (Set.toList signers)
    SPlutusV2 -> PV2.txInfoSignatories txInfo `shouldBe` fmap transKeyHash (Set.toList signers)
    SPlutusV3 -> PV3.txInfoSignatories txInfo `shouldBe` fmap transKeyHash (Set.toList signers)
    SPlutusV4 -> PV4.txInfoSignatories txInfo `shouldBe` fmap transKeyHash (Set.toList signers)

expectValidityRange ::
  forall era l.
  EraTranslateValidityInterval era =>
  SLanguage l ->
  ValidityInterval ->
  PlutusTxInfo l ->
  Expectation
expectValidityRange slang vi txInfo =
  case translateVI @era ei ss vi of
    Left _ -> pure () -- Won't happen: fixedEpochInfo has no horizon
    Right tr -> case slang of
      SPlutusV1 -> PV1.txInfoValidRange txInfo `shouldBe` tr
      SPlutusV2 -> PV2.txInfoValidRange txInfo `shouldBe` tr
      SPlutusV3 -> PV3.txInfoValidRange txInfo `shouldBe` tr
      SPlutusV4 -> PV4.txInfoValidRange txInfo `shouldBe` tr

-- | Standalone property test for V1 certificate translation.  Verifies that
-- arbitrary 'TxCert's are correctly reflected in the V1 'TxInfo'.
txInfoCertsSpec ::
  forall era l.
  ( EraTx era
  , EraPlutusTxInfo l era
  , Arbitrary (TxCert era)
  ) =>
  SLanguage l ->
  Spec
txInfoCertsSpec slang =
  prop "correctly translate certificates" $ do
    certs <- arbitrary
    let pv = ProtVer (eraProtVerLow @era) 0
        txBody = mkBasicTxBody & certsTxBodyL .~ certs
        tx = mkBasicTx txBody
    pure $
      successfulTranslation @era slang mempty tx $ \_slang txInfo ->
        expectTxCerts @era pv certs slang txInfo

txInfoSignersSpec ::
  forall era l.
  ( EraTx era
  , AlonzoEraTxBody era
  , AtMostEra "Conway" era
  , EraPlutusTxInfo l era
  ) =>
  SLanguage l ->
  Spec
txInfoSignersSpec slang =
  prop "correctly translate tx signers" $ do
    signers <- arbitrary
    let txBody =
          mkBasicTxBody
            & reqSignerHashesTxBodyL .~ signers
        tx = mkBasicTx txBody
    pure $
      successfulTranslation @era slang mempty tx $ \_slang txInfo ->
        expectSignatories signers slang txInfo

successfulTranslation ::
  forall era l.
  ( EraPlutusTxInfo l era
  ) =>
  SLanguage l ->
  UTxO era ->
  Tx TopTx era ->
  (SLanguage l -> PlutusTxInfo l -> Expectation) ->
  Expectation
successfulTranslation slang utxo tx f =
  let lti =
        LedgerTxInfo
          { ltiProtVer = ProtVer (eraProtVerLow @era) 0
          , ltiEpochInfo = ei
          , ltiSystemStart = ss
          , ltiUTxO = utxo
          , ltiTx = tx
          }
   in case toPlutusTxInfoForPurpose slang lti (SpendingPurpose AsPurpose) of
        Right txInfo -> f slang txInfo
        Left e -> assertFailure $ "no translation error was expected, but got: " <> show e

ei :: EpochInfo (Either a)
ei = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

ss :: SystemStart
ss = SystemStart $ posixSecondsToUTCTime 0

genUtxoEntry :: (AlonzoEraTxOut era, Arbitrary (Value era)) => Gen (TxIn, TxOut era)
genUtxoEntry = do
  txIn <- arbitrary
  txOut <- genTxOut
  pure (txIn, txOut)

genTxOut :: forall era. (AlonzoEraTxOut era, Arbitrary (Value era)) => Gen (TxOut era)
genTxOut = do
  let genAddr =
        -- Byron addresses are not permitted in later eras
        if eraName @era == "Alonzo"
           then arbitrary
           else Addr <$> arbitrary <*> arbitrary <*> arbitrary
  dataHash <- arbitrary
  txOut <-
    mkBasicTxOut
      <$> genAddr
      <*> scale (`div` 15) arbitrary
  pure $ txOut & dataHashTxOutL .~ dataHash
