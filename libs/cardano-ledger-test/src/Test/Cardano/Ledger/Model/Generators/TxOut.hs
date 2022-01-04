{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.Generators.TxOut where

import Cardano.Ledger.Coin (Coin (..))
import qualified Cardano.Ledger.Val as Val
import Control.Lens
  ( ifor,
    to,
    uses,
    _2,
  )
import Control.Monad.Reader.Class (asks)
import Control.Monad.Supply (MonadSupply (..))
import Data.Foldable (toList)
import Data.Group (Group (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Traversable (for)
import QuickCheck.GenT
  ( choose,
    elements,
    liftGen,
    oneof,
    shuffle,
  )
import Test.Cardano.Ledger.Model.API
  ( getModelLedger_utxos,
    modelLedger,
    modelLedger_nes,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
    liftModelValue,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSupport (..),
    IfSupportsMint,
    ScriptFeature,
    TyValueExpected (..),
    ValueFeature,
    reifySupportsMint',
  )
import Test.Cardano.Ledger.Model.Generators
  ( AllowScripts,
    HasGenModelM,
    ModelGeneratorContext (..),
    ModelGeneratorParamsF (..),
  )
import Test.Cardano.Ledger.Model.Generators.Address
  ( freshWdrlAddress,
    genAddr,
  )
import Test.Cardano.Ledger.Model.Generators.Script
  ( genRedeemer,
    genScriptData,
    guardHaveCollateral,
  )
import Test.Cardano.Ledger.Model.Generators.Value
  ( unfoldModelValue,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( modelEpochState_ss,
    modelNewEpochState_es,
    modelSnapshot_delegations,
    modelSnapshots_pstake,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    modelAddress_pmt,
  )
import Test.Cardano.Ledger.Model.Snapshot (snapshotQueue_mark)
import Test.Cardano.Ledger.Model.Tx
  ( ModelMintValue,
    ModelRedeemer,
    ModelScriptPurpose (..),
    mkMintValue,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId (..),
    modelTxOut_address,
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
  )
import Test.Cardano.Ledger.Model.Value
  ( ModelValueF (..),
  )

genInputs ::
  forall st era m.
  HasGenModelM st era m =>
  AllowScripts (ScriptFeature era) ->
  Coin ->
  Coin ->
  m (Map ModelUTxOId (ModelRedeemer (ScriptFeature era), ModelTxOut era))
genInputs allowScripts deposits refunds = do
  actualUtxos <- uses (modelLedger . to getModelLedger_utxos) _modelUTxOMap_utxos
  utxos0 <-
    shuffle
      =<< uses
        (modelLedger . to getModelLedger_utxos)
        (mapMaybe (_2 . modelTxOut_address . modelAddress_pmt $ guardHaveCollateral allowScripts) . Map.toList . _modelUTxOMap_utxos)

  let spendable :: ModelTxOut era -> Coin
      spendable = Val.coin . _mtxo_value

      minInput =
        Coin (minFee + minOutput) <> deposits ~~ refunds
          <> if not (bifoldMapSupportsFeature (const True) id allowScripts) && reifySupportsMint' (Proxy @(ValueFeature era))
            then Coin minOutput
            else mempty

      go :: [(ModelUTxOId, ModelTxOut era)] -> Coin -> [(ModelUTxOId, ModelTxOut era)] -> [(ModelUTxOId, ModelTxOut era)]
      go [] val acc
        | val >= minInput = acc
        | otherwise =
          error $
            unlines
              [ "insufficient UTxO's to proceed with generation.",
                show actualUtxos
              ]
      -- TODO, get rewards/fees back into circulation in generator.
      go (utxo : rest) val acc
        | val < minInput = go rest (val <> spendable (snd utxo)) (utxo : acc)
        | otherwise = acc

  numTxInputs <- liftGen =<< asks (_modelGeneratorParams_numTxInputs . _modelGeneratorContext_modelGeneratorParams)
  let utxos1 = (take numTxInputs utxos0)
      val1 = foldMap (spendable . snd) utxos1
  let ins = Map.fromList $ go (drop numTxInputs utxos0) val1 utxos1
  ifor ins $ \ui txo -> do
    rdmr <- genRedeemer $ ModelScriptPurpose_Spending ui
    pure (rdmr, txo)

genOutputs ::
  HasGenModelM st era m =>
  AllowScripts (ScriptFeature era) ->
  -- | tx inputs
  Map ModelUTxOId (ModelRedeemer (ScriptFeature era), ModelTxOut era) ->
  -- | tx mint
  IfSupportsMint () (ModelMintValue (ScriptFeature era)) (ValueFeature era) ->
  -- | deposits
  Coin ->
  -- | refunds
  Coin ->
  ModelValue 'ExpectAdaOnly era ->
  m ([(ModelUTxOId, ModelTxOut era)], ModelValue 'ExpectAdaOnly era)
genOutputs haveCollateral ins mintWithRdmr deposits refunds wdrls = do
  -- by assumption, there are no rewards; since they would have been outputs to
  -- earlier transactions, and thus have different value now. thus the non-ada
  -- values are entirely known qty multi-asset outputs.
  let mint = mkMintValue mintWithRdmr
      (ModelValueF (Coin inAda, ma)) =
        unModelValue $
          mconcat
            [ mint,
              foldMap (_mtxo_value . snd) ins,
              liftModelValue wdrls,
              Val.inject (refunds ~~ deposits)
            ]
      haveCollateral' =
        bifoldMapSupportsFeature (const True) id haveCollateral
          || ma == mempty
  -- TODO: corner case, if the amount of inAda < minFee + minOutput && ma > 0;
  -- the inputs are unspendable, and the generator needs to abort.
  (fee, outVals) <-
    if
        | inAda < minFee -> error "input too small"
        | inAda < minFee + minOutput && haveCollateral' -> pure (inAda, [])
        | inAda < minFee + (minOutput * 2) && not haveCollateral' -> error "leftover multi-asset"
        | not haveCollateral' -> do
          let maOut = ModelValueF (Coin minOutput, ma)
          fee <- choose (minFee, min (inAda - minOutput) maxFee)
          outVals <- liftGen $ unfoldModelValue (Coin minOutput) (ModelValueF (Coin (inAda - fee - minOutput), mempty))
          pure (fee, maOut : toList outVals)
        | otherwise -> do
          fee <- choose (minFee, min (inAda - minOutput) maxFee)
          outVals <-
            liftGen $
              unfoldModelValue
                (Coin minOutput)
                ( ModelValueF
                    ( Coin
                        (inAda - fee),
                      ma
                    )
                )
          pure (fee, toList outVals)

  delegates <-
    uses
      (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ss . modelSnapshots_pstake . snapshotQueue_mark . modelSnapshot_delegations)
      Map.keys

  outs <- for outVals $ \outVal -> do
    -- when (Val.coin outVal < mempty) $
    --   error $ "negative ModelTxOut: " <> show outVals
    ui <- freshUTxOId
    addr <-
      oneof $
        (fmap pure $ mapMaybe ((modelAddress_pmt $ guardHaveCollateral haveCollateral) . _mtxo_address . snd) $ toList ins)
          <> [genAddr haveCollateral "genOutputs"]
          <> if null delegates then [] else [freshWdrlAddress =<< elements delegates]
    dh <- liftGen $ genScriptData $ _modelAddress_pmt addr
    pure (ui, ModelTxOut addr (ModelValue outVal) dh)
  pure (outs, Val.inject $ Coin fee)

freshUTxOId :: (Integral n, MonadSupply n m) => m ModelUTxOId
freshUTxOId = ModelUTxOId . toInteger <$> supply

minOutput :: Integer
minOutput = 2_000_000

minFee :: Integer
minFee = 400_000

maxFee :: Integer
maxFee = 10 * minFee
