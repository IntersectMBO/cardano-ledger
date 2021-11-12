{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Cardano.Ledger.Model.Generators.Script where

import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Control.Lens (at, to, use, uses)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified PlutusTx (Data (..))
import QuickCheck.GenT
  ( Gen,
    arbitrary,
    choose,
    frequency,
    liftGen,
  )
import Test.Cardano.Ledger.Model.API
  ( getModelLedger_utxos,
    modelLedger,
    modelLedger_nes,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSupport (..),
    IfSupportsPlutus (..),
    KnownScriptFeature,
    ScriptFeature,
    bifoldMapSupportsFeature,
    ifSupportsPlutus,
    reifySupportsPlutus,
    traverseSupportsFeature,
  )
import Test.Cardano.Ledger.Model.Generators
  ( AllowScripts,
    HasGenModelM,
    chooseElems,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( modelEpochState_ls,
    modelLState_utxoSt,
    modelNewEpochState_es,
    modelUTxOState_utxo,
  )
import Test.Cardano.Ledger.Model.Script (ModelAddress (..), ModelCredential (..), ModelPlutusScript (..), ModelScript (..), PreprocessedPlutusScript (..))
import Test.Cardano.Ledger.Model.Tx
  ( ModelRedeemer,
    ModelScriptPurpose (..),
    modelCWitness,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
  )
import Test.Cardano.Ledger.Model.UTxO (ModelUTxOMap (..))

genRedeemer :: forall era m st. HasGenModelM st era m => ModelScriptPurpose era -> m (ModelRedeemer (ScriptFeature era))
genRedeemer sp = traverseSupportsFeature go $ reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era))
  where
    go () = case sp of
      ModelScriptPurpose_Minting policyId -> case policyId of
        ModelScript_Timelock _ -> pure Nothing
        ModelScript_PlutusV1 sc -> liftGen $ Just <$> genScriptRedeemer Nothing sc
      ModelScriptPurpose_Spending ui ->
        use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo . at ui) >>= \case
          Nothing -> pure Nothing -- this "should" be an error
          Just txo -> liftGen $ genCredRedeemer (bifoldMapSupportsFeature (const Nothing) id $ _mtxo_data txo) (_modelAddress_pmt $ _mtxo_address txo)
      ModelScriptPurpose_Rewarding cred -> liftGen $ genCredRedeemer Nothing cred
      ModelScriptPurpose_Certifying cert -> case modelCWitness cert of
        Nothing -> pure Nothing
        Just cred -> liftGen $ genCredRedeemer Nothing cred

genCredRedeemer :: Maybe PlutusTx.Data -> ModelCredential k sf -> Gen (Maybe (PlutusTx.Data, ExUnits))
genCredRedeemer dh = \case
  ModelKeyHashObj _ -> pure Nothing
  ModelScriptHashObj sc -> Just <$> genScriptRedeemer dh sc

genScriptRedeemer :: Maybe PlutusTx.Data -> ModelPlutusScript -> Gen (PlutusTx.Data, ExUnits)
genScriptRedeemer dh = \case
  ModelPlutusScript_AlwaysSucceeds _ -> pure (PlutusTx.I 1, ExUnits 5 5)
  ModelPlutusScript_AlwaysFails _ -> pure (PlutusTx.I 1, ExUnits 5 5)
  ModelPlutusScript_Salt _ sc -> fmap (<> ExUnits 1 1) <$> genScriptRedeemer dh sc
  ModelPlutusScript_Preprocessed sc -> genPreprocesedScriptRedeemer dh sc

genPreprocesedScriptRedeemer :: Maybe PlutusTx.Data -> PreprocessedPlutusScript -> Gen (PlutusTx.Data, ExUnits)
genPreprocesedScriptRedeemer dh =
  let withDh f = case dh of
        Nothing -> pure (PlutusTx.I 0, ExUnits 5 5)
        Just dh' -> f dh'
      eu = ExUnits 5 5
   in \case
        GuessTheNumber3 -> withDh $ \dh' ->
          frequency
            [ (10, pure $ (dh', eu)),
              (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
            ]
        Evendata3 -> pure (PlutusTx.I 10, eu)
        Odddata3 -> pure (PlutusTx.I 10, eu)
        EvenRedeemer3 ->
          frequency
            [ (10, pure $ (PlutusTx.I 10, eu)),
              (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
            ]
        OddRedeemer3 ->
          frequency
            [ (10, pure $ (PlutusTx.I 11, eu)),
              (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
            ]
        SumsTo103 -> withDh $ \case
          PlutusTx.I dh' ->
            frequency
              [ (10, pure $ (PlutusTx.I $ 10 - dh', eu)),
                (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
              ]
          _ -> pure $ (PlutusTx.I 11, eu)
        OddRedeemer2 ->
          frequency
            [ (10, pure $ (PlutusTx.I 11, eu)),
              (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
            ]
        EvenRedeemer2 ->
          frequency
            [ (10, pure $ (PlutusTx.I 10, eu)),
              (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
            ]
        RedeemerIs102 ->
          frequency
            [ (10, pure $ (PlutusTx.I 10, eu)),
              (1, (,) <$> fmap PlutusTx.I arbitrary <*> pure eu)
            ]

genScriptData :: forall sf r. KnownScriptFeature sf => ModelCredential r sf -> Gen (IfSupportsPlutus () (Maybe PlutusTx.Data) sf)
genScriptData addr = traverseSupportsFeature id $
  ifSupportsPlutus (Proxy :: Proxy sf) () $ case addr of
    ModelKeyHashObj _ -> pure Nothing
    -- ModelScriptHashObj _ -> Just . PlutusTx.I <$> arbitrary
    ModelScriptHashObj _ -> Just . PlutusTx.I <$> pure 0

genCollateral ::
  forall era m st.
  HasGenModelM st era m =>
  m (AllowScripts (ScriptFeature era), IfSupportsPlutus () (Set ModelUTxOId) (ScriptFeature era))
genCollateral = do
  res <- flip traverseSupportsFeature (reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era))) $ \() -> do
    availableCollateralInputs <- uses (modelLedger . to getModelLedger_utxos) $ _modelUTxOMap_collateralUtxos
    numCollateralInputs <- choose (1, min 5 (Set.size availableCollateralInputs - 1))
    (collateral, rest) <- chooseElems numCollateralInputs availableCollateralInputs
    -- avoid spending the last unlocked utxo
    pure $
      if 10 > Set.size rest -- genInputs may use up to 8 inputs.
        then Set.empty
        else collateral

  pure (mapSupportsFeature (not . Set.null) res, res)

guardHaveCollateral ::
  IfSupportsPlutus () Bool sf ->
  ModelCredential k sf ->
  Maybe (ModelCredential k sf)
guardHaveCollateral (SupportsPlutus False) (ModelScriptHashObj _) = Nothing
guardHaveCollateral _ x = Just x
