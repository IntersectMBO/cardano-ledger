{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.Generators.Tx where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Lens
  ( has,
    to,
    use,
    uses,
    (.=),
  )
import Control.Monad (forM, replicateM)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Supply
  ( MonadSupply (..),
  )
import qualified Data.ByteString as BS
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import QuickCheck.GenT
  ( Gen,
    arbitrary,
    choose,
    elements,
    frequency,
    liftGen,
    oneof,
    resize,
    sublistOf,
  )
import Test.Cardano.Ledger.Model.API
  ( applyModelDCert,
    applyModelTx,
    execModelM,
    getModelLedger_rewards,
    getModelLedger_utxos,
    modelLedger,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
    ModelValueVars (..),
    getGlobals,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet (..),
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    KnownScriptFeature,
    ScriptFeature,
    TyScriptFeature (..),
    TyValueExpected (..),
    ValueFeature,
    fromSupportsMint,
    ifSupportsMint,
    ifSupportsPlutus,
    reifySupportsMint,
    reifySupportsPlutus,
    traverseSupportsMint,
  )
import Test.Cardano.Ledger.Model.Fixup (witnessModelTx)
import Test.Cardano.Ledger.Model.Generators
  ( AllowScripts,
    HasGenModelM,
    ModelGeneratorContext (..),
    ModelGeneratorParamsF (..),
    chooseElems,
  )
import Test.Cardano.Ledger.Model.Generators.Certificates
  ( genDCert,
  )
import Test.Cardano.Ledger.Model.Generators.Script
  ( genCollateral,
    guardHaveCollateral,
  )
import Test.Cardano.Ledger.Model.Generators.TxOut
  ( genInputs,
    genOutputs,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential (..),
    ModelPlutusScript (..),
    ModelScript (..),
    PreprocessedPlutusScript (..),
    modelAddress_pmt,
    _ModelScriptHashObj,
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelTx (..),
    modelDelegation_delegator,
    _ModelDelegate,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
    modelTxOut_address,
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
  )
import Test.Cardano.Ledger.Model.Value
  ( getModelValueF,
    mkModelValueF',
  )
import Test.QuickCheck.Instances.ByteString ()

genWdrl :: HasGenModelM st era m => AllowScripts (ScriptFeature era) -> m (Map (ModelCredential 'Staking (ScriptFeature era)) Coin)
genWdrl allowScripts = do
  allRewards <- uses (modelLedger . to getModelLedger_rewards) $ Map.filter (/= Val.zero)
  numWdrls <- liftGen =<< asks (_modelGeneratorParams_numWdrls . _modelGeneratorContext_modelGeneratorParams)
  (rewards, _) <- chooseElems numWdrls $ Map.mapMaybeWithKey (\k v -> v <$ guardHaveCollateral allowScripts k) allRewards
  pure rewards

needCollateral ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Map ModelUTxOId (ModelTxOut era) ->
  Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era) ->
  IfSupportsMint () (ModelValue (ValueFeature era) era) (ValueFeature era) ->
  [ModelDCert ('FeatureSet (ValueFeature era) (ScriptFeature era))] ->
  Bool
needCollateral ins wdrls mint dcerts = case reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)) of
  NoPlutusSupport () -> False
  SupportsPlutus () ->
    has (traverse . modelTxOut_address . modelAddress_pmt . _ModelScriptHashObj) ins
      || has (traverse . _ModelScriptHashObj) (Map.keys wdrls)
      || fromSupportsMint (\() -> False) (any isPlutusMintAsset . unModelValue) mint
      || has (traverse . _ModelDelegate . modelDelegation_delegator . _ModelScriptHashObj) dcerts
  where
    isPlutusMintAsset :: ModelValueVars era (ValueFeature era) -> Bool
    isPlutusMintAsset (ModelValue_MA (ModelScript_PlutusV1 _, _)) = True
    isPlutusMintAsset _ = False

wouldSpendLastCollateral :: HasGenModelM st era m => SlotNo -> ModelTx era -> m Bool
wouldSpendLastCollateral slot txn = do
  g <- asks getGlobals
  ml' <- uses modelLedger (execModelM (applyModelTx slot 999 txn) g)
  pure . Set.null . _modelUTxOMap_collateralUtxos $ getModelLedger_utxos ml'

freshMintScript ::
  forall era st m tlf.
  ( HasGenModelM st era m,
    ScriptFeature era ~ 'TyScriptFeature tlf 'True
  ) =>
  m (ModelScript (ScriptFeature era))
freshMintScript = do
  x <- supply
  case (reifySupportsPlutus (Proxy @(ScriptFeature era))) of
    SupportsPlutus () ->
      pure $
        ModelScript_PlutusV1 $
          ModelPlutusScript_Salt x $ ModelPlutusScript_Preprocessed RedeemerIs102

genAssetName :: Gen AssetName
genAssetName =
  AssetName
    <$> oneof
      [ pure BS.empty,
        resize 32 arbitrary
      ]

freshAsset ::
  forall era st m tlf.
  ( HasGenModelM st era m,
    ScriptFeature era ~ 'TyScriptFeature tlf 'True,
    ValueFeature era ~ 'ExpectAnyOutput
  ) =>
  m (ModelValueVars era 'ExpectAnyOutput)
freshAsset =
  case (reifySupportsMint (Proxy @(ValueFeature era))) of
    SupportsMint () ->
      fmap ModelValue_MA ((,) <$> freshMintScript <*> liftGen genAssetName)

makeAsset ::
  forall era st m.
  HasGenModelM st era m =>
  (ModelValueVars era 'ExpectAnyOutput) ->
  m (ModelValue 'ExpectAnyOutput era)
makeAsset asset = do
  numAssetsToMint <- choose (1, 10)
  newAssets <-
    replicateM numAssetsToMint $
      (,) asset
        <$> frequency
          [ (1, pure 1),
            (1, choose (1_000 :: Integer, 1_000_000))
          ]
  pure $ ModelValue $ mkModelValueF' mempty $ Map.fromList newAssets

data MintBurnAmount
  = MintBurnSmallest
  | MintBurnEverything
  | MintBurnRandom

mintBurnAssets ::
  forall era st m.
  HasGenModelM st era m =>
  [(ModelValueVars era 'ExpectAnyOutput, Integer)] ->
  m (ModelValue 'ExpectAnyOutput era)
mintBurnAssets assets = do
  burnAmt <- elements [MintBurnRandom, MintBurnEverything, MintBurnSmallest]
  modifiedAssets <- forM assets $ \(asset, qty) ->
    fmap ((,) asset) $
      case burnAmt of
        MintBurnSmallest -> pure (-1)
        MintBurnEverything -> pure (-qty)
        MintBurnRandom -> choose (-qty, -1 :: Integer)

  pure $ ModelValue $ mkModelValueF' mempty $ Map.fromList modifiedAssets

genMint ::
  forall era st m tlf.
  ( HasGenModelM st era m,
    ScriptFeature era ~ 'TyScriptFeature tlf 'True,
    ValueFeature era ~ 'ExpectAnyOutput
  ) =>
  ModelValue (ValueFeature era) era ->
  m (ModelValue (ValueFeature era) era)
genMint inputVal = do
  utxoMap <- uses modelLedger getModelLedger_utxos
  let getMapFromModelValue = snd . getModelValueF . unModelValue
      existingAssets = (Map.keys . getMapFromModelValue . _modelUTxOMap_balance) utxoMap
      inputAssets = (Map.toList . getMapFromModelValue) inputVal
  frequency $
    [ (1, pure mempty),
      (1, makeAsset =<< freshAsset)
    ]
      <> [(1, makeAsset =<< elements existingAssets) | (not . null) existingAssets]
      <> [ ( 1,
             do
               sublistInputs <- sublistOf inputAssets
               randAssets <-
                 frequency $
                   [(1, pure sublistInputs) | (not . null) sublistInputs]
                     <> [ (1, fmap (: []) $ elements inputAssets),
                          (1, pure inputAssets)
                        ]
               mintBurnAssets randAssets
           )
           | (not . null) inputAssets
         ]

genModelTx :: forall era m st. SlotNo -> HasGenModelM st era m => m (ModelTx era)
genModelTx slot = do
  (haveCollateral, collateral) <- genCollateral
  ins <- genInputs haveCollateral
  wdrl <- fmap Val.inject <$> genWdrl haveCollateral

  mint <- case haveCollateral of
    NoPlutusSupport () -> pure mempty
    SupportsPlutus False -> pure mempty
    SupportsPlutus True ->
      traverseSupportsMint id $
        ifSupportsMint
          (Proxy :: Proxy (ValueFeature era))
          ()
          (genMint $ foldMap _mtxo_value ins)

  (outs, fee) <- genOutputs haveCollateral ins mint
  let txn =
        ModelTx
          { _mtxInputs = Map.keysSet ins,
            _mtxOutputs = outs,
            _mtxFee = fee <> fold wdrl, -- TODO, put withdwrawals in outputs sometimes.
            _mtxDCert = [],
            _mtxWdrl = wdrl,
            _mtxMint = mint,
            _mtxCollateral = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Set.empty,
            _mtxValidity = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () (IsValid True),
            _mtxRedeemers = Map.empty,
            _mtxWitnessSigs = Set.empty
          }

  dcerts <- do
    st0 <- use modelLedger
    applyModelTx slot 999 txn -- bogus txIx; we rewind after we've finished generating the tx.
    numDCerts <- liftGen =<< asks (_modelGeneratorParams_numDCerts . _modelGeneratorContext_modelGeneratorParams)
    dcerts <- replicateM numDCerts $ do
      dcert <- genDCert haveCollateral
      applyModelDCert dcert
      pure dcert

    modelLedger .= st0

    pure dcerts

  let nc = needCollateral ins wdrl mint dcerts
  uses modelLedger $
    witnessModelTx
      txn
        { _mtxDCert = dcerts,
          _mtxCollateral = if nc then collateral else _mtxCollateral txn
        }
