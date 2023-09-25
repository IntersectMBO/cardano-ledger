{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Core.Binary where

import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (EqRaw (eqRaw))
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Binary.TreeDiff (diffExpr)
import Test.Cardano.Ledger.Common

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
  ) =>
  Spec
specTxAuxDataUpgrade =
  prop "upgradeTxAuxData is preserved through serialization" $ \prevTxAuxData -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevTxAuxData of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxAuxData :: TxAuxData era) -> do
        let upgradedTxAuxData = upgradeTxAuxData prevTxAuxData
        unless (eqRaw curTxAuxData upgradedTxAuxData) $
          expectationFailure $
            "Expected raw representation of TxAuxData to be equal: \n"
              <> diffExpr curTxAuxData upgradedTxAuxData

specScriptUpgrade ::
  forall era.
  ( EraScript (PreviousEra era)
  , EraScript era
  , Arbitrary (Script (PreviousEra era))
  , HasCallStack
  ) =>
  Spec
specScriptUpgrade =
  prop "upgradeScript is preserved through serialization" $ \prevScript -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevScript of
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
  ) =>
  Spec
specTxWitsUpgrade =
  prop "upgradeTxWits is preserved through serialization" $ \prevTxWits -> do
    case embedTripAnn (eraProtVerHigh @(PreviousEra era)) (eraProtVerLow @era) prevTxWits of
      Left err ->
        expectationFailure $
          "Expected to deserialize: =======================================================\n"
            ++ show err
      Right (curTxWits :: TxWits era) -> do
        let upgradedTxWits = upgradeTxWits prevTxWits
        unless (eqRaw curTxWits upgradedTxWits) $
          expectationFailure $
            "Expected raw representation of TxWits to be equal: \n"
              <> diffExpr curTxWits upgradedTxWits

specTxBodyUpgrade ::
  forall era.
  ( EraTxBody (PreviousEra era)
  , EraTxBody era
  , Arbitrary (TxBody (PreviousEra era))
  , HasCallStack
  ) =>
  Spec
specTxBodyUpgrade =
  prop "upgradeTxBody is preserved through serialization" $ \prevTxBody -> do
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
          unless (eqRaw curTxBody upgradedTxBody) $
            expectationFailure $
              "Expected raw representation of TxBody to be equal: \n"
                <> diffExpr curTxBody upgradedTxBody
        | otherwise -> expectationFailure "Expected upgradeTxBody to succeed"

specTxUpgrade ::
  forall era.
  ( EraTx (PreviousEra era)
  , EraTx era
  , Arbitrary (Tx (PreviousEra era))
  , HasCallStack
  ) =>
  Spec
specTxUpgrade =
  prop "upgradeTx is preserved through serialization" $ \prevTx -> do
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
          unless (eqRaw curTx upgradedTx) $
            expectationFailure $
              "Expected raw representation of Tx to be equal: \n"
                <> diffExpr curTx upgradedTx
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
  ) =>Bool ->
  Spec
specUpgrade isScriptUpgradeable =
  describe ("Upgrade from " ++ eraName @(PreviousEra era) ++ " to " ++ eraName @era) $ do
    specTxOutUpgrade @era
    specTxCertUpgrade @era
    specTxAuxDataUpgrade @era
    specTxWitsUpgrade @era
    specTxBodyUpgrade @era
    specTxUpgrade @era
    when isScriptUpgradeable $
      specScriptUpgrade @era
