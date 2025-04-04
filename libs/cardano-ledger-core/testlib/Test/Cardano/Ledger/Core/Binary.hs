{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Core.Binary (
  BinaryUpgradeOpts (..),
  decoderEquivalenceCoreEraTypesSpec,
  specUpgrade,
  decoderEquivalenceEraSpec,
  txSizeSpec,
) where

import Cardano.Ledger.Binary (DecCBOR, ToCBOR, decNoShareCBOR, encodeMemPack)
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (EqRaw (eqRaw))
import Cardano.Ledger.Plutus (Data)
import Data.Default (Default (def))
import Lens.Micro
import qualified Prettyprinter as Pretty
import Test.Cardano.Ledger.Binary (decoderEquivalenceSpec)
import Test.Cardano.Ledger.Binary.Annotator
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.TreeDiff (AnsiStyle, Doc)

data BinaryUpgradeOpts = BinaryUpgradeOpts
  { isScriptUpgradeable :: Bool
  , isTxUpgradeable :: Bool
  }

instance Default BinaryUpgradeOpts where
  def =
    BinaryUpgradeOpts
      { isScriptUpgradeable = True
      , isTxUpgradeable = True
      }

specTxOutUpgrade ::
  forall era.
  ( EraTxOut (PreviousEra era)
  , EraTxOut era
  , Arbitrary (TxOut (PreviousEra era))
  , HasCallStack
  ) =>
  Spec
specTxOutUpgrade =
  prop "upgradeTxOut is preserved through serialization" $ \prevTxOut -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevTxOut of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxOut :: TxOut era) ->
        curTxOut `shouldBe` upgradeTxOut prevTxOut

specTxCertUpgrade ::
  forall era.
  ( EraTxCert (PreviousEra era)
  , EraTxCert era
  , Arbitrary (TxCert (PreviousEra era))
  , HasCallStack
  ) =>
  Spec
specTxCertUpgrade =
  prop "upgradeTxCert is preserved through serialization" $ \prevTxCert -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevTxCert of
      Left err
        | Right _ <- upgradeTxCert prevTxCert ->
            -- We expect deserialization to succeed, when upgrade is possible
            expectationFailure $
              "Expected to deserialize: =======================================================\n"
                ++ show err
        | otherwise -> pure () -- Both upgrade and deserializer fail successfully
      Right (curTxCert :: TxCert era)
        | Right upgradedTxCert <- upgradeTxCert prevTxCert ->
            curTxCert `shouldBe` upgradedTxCert
        | otherwise -> expectationFailure "Expected upgradeTxCert to succeed"

specTxAuxDataUpgrade ::
  forall era.
  ( EraTxAuxData (PreviousEra era)
  , EraTxAuxData era
  , Arbitrary (TxAuxData (PreviousEra era))
  , HasCallStack
  , ToExpr (TxAuxData era)
  , DecCBOR (TxAuxData era)
  ) =>
  Spec
specTxAuxDataUpgrade = do
  prop "upgradeTxAuxData is preserved through serialization (Annotator)" $ \prevTxAuxData -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevTxAuxData of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxAuxData :: TxAuxData era) -> do
        let upgradedTxAuxData = upgradeTxAuxData prevTxAuxData
        expectRawEqual "TxAuxData" curTxAuxData upgradedTxAuxData
  prop "upgradeTxAuxData is preserved through serialization" $ \prevTxAuxData -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevTxAuxData of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxAuxData :: TxAuxData era) -> do
        let upgradedTxAuxData = upgradeTxAuxData prevTxAuxData
        expectRawEqual "TxAuxData" curTxAuxData upgradedTxAuxData

specScriptUpgrade ::
  forall era.
  ( EraScript (PreviousEra era)
  , EraScript era
  , Arbitrary (Script (PreviousEra era))
  , DecCBOR (Script era)
  , HasCallStack
  ) =>
  Spec
specScriptUpgrade = do
  prop "upgradeScript is preserved through serialization (Annotator)" $ \prevScript -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevScript of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curScript :: Script era) ->
        curScript `shouldBe` upgradeScript prevScript
  prop "upgradeScript is preserved through serialization" $ \prevScript -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevScript of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curScript :: Script era) ->
        curScript `shouldBe` upgradeScript prevScript

specTxWitsUpgrade ::
  forall era.
  ( EraTxWits (PreviousEra era)
  , EraTxWits era
  , Arbitrary (TxWits (PreviousEra era))
  , HasCallStack
  , ToExpr (TxWits era)
  , DecCBOR (TxWits era)
  ) =>
  Spec
specTxWitsUpgrade = do
  prop "upgradeTxWits is preserved through serialization (Annotator)" $ \prevTxWits -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevTxWits of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxWits :: TxWits era) -> do
        let upgradedTxWits = upgradeTxWits prevTxWits
        expectRawEqual "TxWits" curTxWits upgradedTxWits
  prop "upgradeTxWits is preserved through serialization" $ \prevTxWits -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevTxWits of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxWits :: TxWits era) -> do
        let upgradedTxWits = upgradeTxWits prevTxWits
        expectRawEqual "TxWits" curTxWits upgradedTxWits

specTxBodyUpgrade ::
  forall era.
  ( EraTxBody (PreviousEra era)
  , EraTxBody era
  , Arbitrary (TxBody (PreviousEra era))
  , HasCallStack
  , ToExpr (TxBody era)
  , DecCBOR (TxBody era)
  ) =>
  Spec
specTxBodyUpgrade = do
  prop "upgradeTxBody is preserved through serialization (Annotator)" $ \prevTxBody -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevTxBody of
      Left err
        | Right _ <- upgradeTxBody prevTxBody ->
            -- We expect deserialization to succeed, when upgrade is possible
            expectationFailure $
              "Expected to deserialize: =======================================================\n"
                ++ show err
        | otherwise -> pure () -- Both upgrade and deserializer fail successfully
      Right (curTxBody :: TxBody era)
        | Right upgradedTxBody <- upgradeTxBody prevTxBody ->
            expectRawEqual "TxBody" curTxBody upgradedTxBody
        | otherwise -> expectationFailure "Expected upgradeTxBody to succeed"
  prop "upgradeTxBody is preserved through serialization" $ \prevTxBody -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevTxBody of
      Left err
        | Right _ <- upgradeTxBody prevTxBody ->
            -- We expect deserialization to succeed, when upgrade is possible
            expectationFailure $
              "Expected to deserialize: =======================================================\n"
                ++ show err
        | otherwise -> pure () -- Both upgrade and deserializer fail successfully
      Right (curTxBody :: TxBody era)
        | Right upgradedTxBody <- upgradeTxBody prevTxBody ->
            expectRawEqual "TxBody" curTxBody upgradedTxBody
        | otherwise -> expectationFailure "Expected upgradeTxBody to succeed"

specTxUpgrade ::
  forall era.
  ( EraTx (PreviousEra era)
  , EraTx era
  , Arbitrary (Tx (PreviousEra era))
  , HasCallStack
  , ToExpr (Tx era)
  , DecCBOR (Tx era)
  ) =>
  Spec
specTxUpgrade = do
  prop "upgradeTx is preserved through serialization (Annotator)" $ \prevTx -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevTx of
      Left err
        | Right _ <- upgradeTx prevTx ->
            -- We expect deserialization to succeed, when upgrade is possible
            expectationFailure $
              "Expected to deserialize: =======================================================\n"
                ++ show err
        | otherwise -> pure () -- Both upgrade and deserializer fail successfully
      Right (curTx :: Tx era)
        | Right upgradedTx <- upgradeTx prevTx ->
            expectRawEqual "Tx" curTx upgradedTx
        | otherwise -> expectationFailure "Expected upgradeTx to succeed"
  prop "upgradeTx is preserved through serialization" $ \prevTx -> do
    case embedTrip (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) cborTrip prevTx of
      Left err
        | Right _ <- upgradeTx prevTx ->
            -- We expect deserialization to succeed, when upgrade is possible
            expectationFailure $
              "Expected to deserialize: =======================================================\n"
                ++ show err
        | otherwise -> pure () -- Both upgrade and deserializer fail successfully
      Right (curTx :: Tx era)
        | Right upgradedTx <- upgradeTx prevTx ->
            expectRawEqual "Tx" curTx upgradedTx
        | otherwise -> expectationFailure "Expected upgradeTx to succeed"

specUpgrade ::
  forall era.
  ( Arbitrary (TxOut (PreviousEra era))
  , Arbitrary (TxCert (PreviousEra era))
  , Arbitrary (TxAuxData (PreviousEra era))
  , Arbitrary (TxWits (PreviousEra era))
  , Arbitrary (TxBody (PreviousEra era))
  , EraTx (PreviousEra era)
  , EraTx era
  , Arbitrary (Tx (PreviousEra era))
  , Arbitrary (Script (PreviousEra era))
  , HasCallStack
  , ToExpr (Tx era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  , ToExpr (TxAuxData era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (Script era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxBody era)
  , DecCBOR (Tx era)
  ) =>
  BinaryUpgradeOpts ->
  Spec
specUpgrade BinaryUpgradeOpts {isScriptUpgradeable, isTxUpgradeable} =
  describe ("Upgrade from " ++ eraName @(PreviousEra era) ++ " to " ++ eraName @era) $ do
    specTxOutUpgrade @era
    specTxCertUpgrade @era
    specTxAuxDataUpgrade @era
    specTxWitsUpgrade @era
    specTxBodyUpgrade @era
    when isTxUpgradeable $
      specTxUpgrade @era
    when isScriptUpgradeable $
      specScriptUpgrade @era
    -- This is a test that ensures that binary version of a TxOut is backwards compatible as it is
    -- stored in the ledger state. This property is only important for using MemPack with UTxOHD
    embedTripSpec
      (eraProtVerHigh @(PreviousEra era))
      (eraProtVerLow @era)
      (mkTrip encodeMemPack decNoShareCBOR)
      $ \(txOutCur :: TxOut era) (txOutPrev :: TxOut (PreviousEra era)) ->
        upgradeTxOut txOutPrev `shouldBe` txOutCur

expectRawEqual :: (EqRaw a, ToExpr a, HasCallStack) => Doc AnsiStyle -> a -> a -> Expectation
expectRawEqual thing expected actual =
  unless (eqRaw expected actual) $
    assertColorFailure . ansiDocToString $
      Pretty.vsep
        [ Pretty.hsep ["Expected raw representation of", thing, "to be equal:"]
        , Pretty.indent 2 $ diffExpr expected actual
        ]

decoderEquivalenceEraSpec ::
  forall era t.
  ( Era era
  , Eq t
  , ToCBOR t
  , DecCBOR (Annotator t)
  , Arbitrary t
  , Show t
  ) =>
  Spec
decoderEquivalenceEraSpec = decoderEquivalenceSpec @t (eraProtVerLow @era) (eraProtVerHigh @era)

decoderEquivalenceCoreEraTypesSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (Script era)
  , HasCallStack
  ) =>
  Spec
decoderEquivalenceCoreEraTypesSpec =
  describe "DecCBOR instances equivalence" $ do
    decoderEquivalenceEraSpec @era @(Data era)
    decoderEquivalenceEraSpec @era @(Script era)
    decoderEquivalenceEraSpec @era @(TxAuxData era)
    decoderEquivalenceEraSpec @era @(TxWits era)
    decoderEquivalenceEraSpec @era @(TxBody era)
    decoderEquivalenceEraSpec @era @(Tx era)

txSizeSpec ::
  forall era.
  ( EraTx era
  , Arbitrary (Tx era)
  , SafeToHash (TxWits era)
  ) =>
  Spec
txSizeSpec =
  describe "Transaction size" $ do
    prop "should match the size of the cbor encoding" $ \(tx :: Tx era) -> do
      let txSize = sizeTxForFeeCalculation tx
      txSize `shouldBe` tx ^. sizeTxF
