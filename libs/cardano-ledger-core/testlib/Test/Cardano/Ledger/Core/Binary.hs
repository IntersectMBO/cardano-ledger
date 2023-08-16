{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Core.Binary where

import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (Memoized (RawType), zipMemoRawType)
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Binary.TreeDiff (ToExpr, expectExprEqual)
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

-- The reason why we need to pass `t` as a type argument here is because `TxAuxData` is a
-- type family, so we don't know if the final type will be of the kind `Type -> Type` and
-- can be used with MemoBytes, which requires `t` to be of such kind, because it is later
-- applied to `era`.
specTxAuxDataUpgrade ::
  forall era t.
  ( EraTxAuxData (PreviousEra era)
  , EraTxAuxData era
  , t era ~ TxAuxData era
  , Memoized t
  , Eq (RawType t era)
  , ToExpr (RawType t era)
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
      Right (curTxAuxData :: t era) ->
        -- We need to do all this MemoBytes trickery because underlying bytes and thus the
        -- equality of the same type will no longer be the same, despite that the value will
        zipMemoRawType @t @t expectExprEqual curTxAuxData (upgradeTxAuxData prevTxAuxData)

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
  forall era t.
  ( EraTxWits (PreviousEra era)
  , EraTxWits era
  , t era ~ TxWits era
  , Memoized t
  , Eq (RawType t era)
  , ToExpr (RawType t era)
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
      Right (curTxWits :: TxWits era) ->
        -- We need to do all this MemoBytes trickery because underlying bytes and thus the
        -- equality of the same type will no longer be the same, despite that the value will
        zipMemoRawType @t @t expectExprEqual curTxWits (upgradeTxWits prevTxWits)

specTxBodyUpgrade ::
  forall era t.
  ( EraTxBody (PreviousEra era)
  , EraTxBody era
  , t era ~ TxBody era
  , Memoized t
  , Eq (RawType t era)
  , ToExpr (RawType t era)
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
            -- We need to do all this MemoBytes trickery because underlying bytes and thus the
            -- equality of the same type will no longer be the same, despite that the value will
            zipMemoRawType @t @t expectExprEqual curTxBody upgradedTxBody
        | otherwise -> expectationFailure "Expected upgradeTxBody to succeed"

specTxUpgrade ::
  forall era t.
  ( EraTx (PreviousEra era)
  , EraTx era
  , t era ~ Tx era
  , Memoized t
  , Eq (RawType t era)
  , ToExpr (RawType t era)
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
            -- We need to do all this MemoBytes trickery because underlying bytes and thus the
            -- equality of the same type will no longer be the same, despite that the value will
            zipMemoRawType @t @t expectExprEqual curTx upgradedTx
        | otherwise -> expectationFailure "Expected upgradeTx to succeed"

specUpgrade ::
  forall era txAuxData txWits txBody tx.
  ( Arbitrary (TxOut (PreviousEra era))
  , Arbitrary (TxCert (PreviousEra era))
  , txAuxData era ~ TxAuxData era -- See specTxAuxDataUpgrade for `txAuxData` explanation.
  , Memoized txAuxData
  , Eq (RawType txAuxData era)
  , ToExpr (RawType txAuxData era)
  , Arbitrary (TxAuxData (PreviousEra era))
  , Arbitrary (Script (PreviousEra era))
  , Arbitrary (TxWits (PreviousEra era))
  , txWits era ~ TxWits era
  , Memoized txWits
  , Eq (RawType txWits era)
  , ToExpr (RawType txWits era)
  , Arbitrary (TxBody (PreviousEra era))
  , txBody era ~ TxBody era
  , Memoized txBody
  , Eq (RawType txBody era)
  , ToExpr (RawType txBody era)
  , EraTx (PreviousEra era)
  , EraTx era
  , Arbitrary (Tx (PreviousEra era))
  , tx era ~ Tx era
  , Memoized tx
  , Eq (RawType tx era)
  , ToExpr (RawType tx era)
  , HasCallStack
  ) =>
  Bool ->
  Spec
specUpgrade isScriptUpgradeable =
  describe ("Upgrade from " ++ eraName @(PreviousEra era) ++ " to " ++ eraName @era) $ do
    specTxOutUpgrade @era
    specTxCertUpgrade @era
    specTxAuxDataUpgrade @era @txAuxData
    specTxWitsUpgrade @era @txWits
    specTxBodyUpgrade @era @txBody
    specTxUpgrade @era
    when isScriptUpgradeable $
      specScriptUpgrade @era
