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
      Left err -> expectationFailure $ "Expected to deserialize: " ++ show err
      Right (curTxOut :: TxOut era) ->
        curTxOut `shouldBe` upgradeTxOut prevTxOut

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

specUpgrade ::
  forall era t.
  ( EraTxOut (PreviousEra era)
  , EraTxOut era
  , Arbitrary (TxOut (PreviousEra era))
  , EraTxAuxData (PreviousEra era)
  , EraTxAuxData era
  , t era ~ TxAuxData era
  , Memoized t
  , Eq (RawType t era)
  , ToExpr (RawType t era)
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
    specTxAuxDataUpgrade @era @t
    when isScriptUpgradeable $
      specScriptUpgrade @era
