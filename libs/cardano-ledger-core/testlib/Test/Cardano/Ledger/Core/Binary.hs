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

-- The reason why we need to pass `t` as a type argument here is because `TxAuxData` is a
-- type family, so we don't know if the final type will be of the kind `Type -> Type` and
-- can be used with MemoBytes, which requires `t` to be of such kind, because it is later
-- applied to `era`.
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

specUpgrade ::
  forall era.
  ( EraTxOut (PreviousEra era)
  , EraTxOut era
  , Arbitrary (TxOut (PreviousEra era))
  , EraTxCert (PreviousEra era)
  , EraTxCert era
  , Arbitrary (TxCert (PreviousEra era))
  , EraTxAuxData (PreviousEra era)
  , EraTxAuxData era
  , Arbitrary (TxAuxData (PreviousEra era))
  , EraScript (PreviousEra era)
  , EraScript era
  , Arbitrary (Script (PreviousEra era))
  , HasCallStack
  ) =>
  Bool ->
  Spec
specUpgrade isScriptUpgradeable =
  describe ("Upgrade from " ++ eraName @(PreviousEra era) ++ " to " ++ eraName @era) $ do
    specTxOutUpgrade @era
    specTxCertUpgrade @era
    specTxAuxDataUpgrade @era
    when isScriptUpgradeable $
      specScriptUpgrade @era
