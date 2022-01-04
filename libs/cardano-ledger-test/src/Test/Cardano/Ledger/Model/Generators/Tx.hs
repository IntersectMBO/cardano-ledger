{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.Generators.Tx where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Lens
  ( at,
    ifor,
    preview,
    to,
    use,
    uses,
    (.=),
    (<<.=),
    _Nothing,
  )
import Control.Monad (replicateM)
import Control.Monad.Reader.Class (asks)
import qualified Control.Monad.State.Strict as State
import Control.Monad.Supply
  ( MonadSupply (..),
  )
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
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
    modelLedger_nes,
  )
import Test.Cardano.Ledger.Model.BaseTypes
  ( ModelValue (..),
    ModelValueVars (..),
    getGlobals,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureSet (..),
    FeatureSupport (..),
    IfSupportsMint (..),
    IfSupportsPlutus (..),
    KnownScriptFeature,
    ScriptFeature,
    TyScriptFeature (..),
    TyValueExpected (..),
    ValueFeature,
    ifSupportsMint,
    ifSupportsPlutus,
    reifySupportsMint,
    reifySupportsPlutus,
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
    genRedeemer,
    guardHaveCollateral,
  )
import Test.Cardano.Ledger.Model.Generators.TxOut
  ( genInputs,
    genOutputs,
  )
import Test.Cardano.Ledger.Model.LedgerState
  ( modelDPState_pstate,
    modelEpochState_ls,
    modelLState_dpstate,
    modelLState_utxoSt,
    modelNewEpochState_es,
    modelPState_poolParams,
    modelUTxOState_utxo,
    validateModelTx,
  )
import Test.Cardano.Ledger.Model.PParams
  ( getModelPParams,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential (..),
    ModelPlutusScript (..),
    ModelScript (..),
    PreprocessedPlutusScript (..),
  )
import Test.Cardano.Ledger.Model.Tx
  ( ModelDCert (..),
    ModelMintValue,
    ModelRedeemer,
    ModelScriptPurpose (..),
    ModelTx (..),
    modelKeyRefunds,
    modelRedeemerPresent,
    modelTotalDeposits,
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelTxOut (..),
    ModelUTxOId,
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
  )
import Test.Cardano.Ledger.Model.Value (getModelValueF)
import Test.QuickCheck.Instances.ByteString ()

genWdrl ::
  HasGenModelM st era m =>
  AllowScripts (ScriptFeature era) ->
  m
    ( Map
        (ModelCredential 'Staking (ScriptFeature era))
        (ModelValue 'ExpectAdaOnly era, ModelRedeemer (ScriptFeature era))
    )
genWdrl allowScripts = do
  allRewards <- uses (modelLedger . to getModelLedger_rewards) $ Map.filter (/= Val.zero)
  numWdrls <- liftGen =<< asks (_modelGeneratorParams_numWdrls . _modelGeneratorContext_modelGeneratorParams)
  (rewards, _) <- chooseElems numWdrls $ Map.mapMaybeWithKey (\k v -> v <$ guardHaveCollateral allowScripts k) allRewards
  ifor rewards $ \cred qty -> do
    rdmr <- genRedeemer (ModelScriptPurpose_Rewarding cred)
    pure (Val.inject qty, rdmr)

needCollateral ::
  forall era.
  KnownScriptFeature (ScriptFeature era) =>
  Map ModelUTxOId (ModelRedeemer (ScriptFeature era), ModelTxOut era) ->
  Map (ModelCredential 'Staking (ScriptFeature era)) (ModelValue 'ExpectAdaOnly era, ModelRedeemer (ScriptFeature era)) ->
  IfSupportsMint () (ModelMintValue (ScriptFeature era)) (ValueFeature era) ->
  [(ModelDCert ('FeatureSet (ValueFeature era) (ScriptFeature era)), ModelRedeemer (ScriptFeature era))] ->
  Bool
needCollateral ins wdrls mint dcerts = case reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era)) of
  NoPlutusSupport () -> False
  SupportsPlutus () ->
    any (modelRedeemerPresent . fst) ins
      || any (modelRedeemerPresent . snd) wdrls
      || bifoldMapSupportsFeature (const False) (any (modelRedeemerPresent . snd)) mint
      || any (modelRedeemerPresent . snd) dcerts

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
  m (ModelMintValue (ScriptFeature era))
makeAsset (ModelValue_MA (policyId, aname)) = do
  rdmr <- genRedeemer (ModelScriptPurpose_Minting policyId)
  qty <-
    frequency
      [ (1, pure 1),
        (1, choose (1_000 :: Integer, 1_000_000))
      ]
  pure $ Map.singleton policyId (Map.singleton aname qty, rdmr)

mintBurnAssets ::
  forall era st m.
  HasGenModelM st era m =>
  NonEmpty (ModelValueVars era 'ExpectAnyOutput, Integer) ->
  -- m (ModelValue 'ExpectAnyOutput era)
  m (ModelMintValue (ScriptFeature era))
mintBurnAssets assets' = do
  let assets = foldMap mkMintQty assets'
      mkMintQty ::
        (ModelValueVars era 'ExpectAnyOutput, Integer) ->
        Map.Map (ModelScript (ScriptFeature era)) (Map.Map AssetName Integer)
      mkMintQty = \case
        (ModelValue_MA (policyId, aname), qty) ->
          Map.singleton policyId $ Map.singleton aname qty
  modifiedAssets <-
    oneof
      [ mkMintQty <$> elements (toList assets'), -- pick one asset, burn one unit of that asset
        pure $ (fmap . fmap) negate assets, -- burn all of it
        (traverse . traverse) (\qty -> choose (-qty, -1 :: Integer)) assets -- burn a random amount of each asset
      ]

  ifor modifiedAssets $ \policy qty -> (,) qty <$> genRedeemer (ModelScriptPurpose_Minting policy)

-- TODO: the Plutus Support constraint is a limitation fo the generator; it
-- should be able to generate any type of asset, not just plutus scripted
-- assets.
genMint ::
  forall era st m tlf.
  ( HasGenModelM st era m,
    ScriptFeature era ~ 'TyScriptFeature tlf 'True
    -- ValueFeature era ~ 'ExpectAnyOutput
  ) =>
  ModelValue (ValueFeature era) era ->
  -- m (ModelValue (ValueFeature era) era)
  m (IfSupportsMint () (ModelMintValue (ScriptFeature era)) (ValueFeature era))
genMint inputVal = sequenceSupportsFeature $
  ifSupportsMint (Proxy :: Proxy (ValueFeature era)) () $ do
    utxoMap <- uses modelLedger getModelLedger_utxos
    let getMapFromModelValue = snd . getModelValueF . unModelValue
        existingAssets = (Map.keys . getMapFromModelValue . _modelUTxOMap_balance) utxoMap
        inputAssets = (Map.toList . getMapFromModelValue) inputVal
    frequency $
      concat
        [ [(1, pure mempty)],
          [(1, makeAsset =<< freshAsset)],
          [(1, makeAsset =<< elements existingAssets) | (not . null) existingAssets],
          case nonEmpty inputAssets of
            Nothing -> []
            Just inputAssets' ->
              [ ( 1,
                  do
                    sublistInputs <- sublistOf inputAssets
                    randAssets <-
                      frequency $
                        concat
                          [ [(1, pure sublistInputs') | sublistInputs' <- toList $ nonEmpty sublistInputs],
                            [(1, fmap pure $ elements inputAssets)],
                            [(1, pure inputAssets')]
                          ]
                    mintBurnAssets randAssets
                )
              ]
        ]

genModelTx :: forall era m st. HasGenModelM st era m => m (ModelTx era)
genModelTx = do
  (haveCollateral, collateral) <- genCollateral
  dcerts <- do
    st0 <- use modelLedger
    -- applyModelTx slot 999 txn -- bogus txIx; we rewind after we've finished generating the tx.
    numDCerts <- liftGen =<< asks (_modelGeneratorParams_numDCerts . _modelGeneratorContext_modelGeneratorParams)
    dcerts <- replicateM numDCerts $ do
      dcert <- genDCert haveCollateral
      applyModelDCert (fst dcert)
      pure dcert
    modelLedger .= st0

    pure dcerts

  let dcertsWithoutRdmr = fmap fst dcerts
  deposits <-
    modelTotalDeposits
      <$> use (modelLedger . modelLedger_nes . to getModelPParams)
      <*> use
        ( modelLedger . modelLedger_nes . modelNewEpochState_es
            . modelEpochState_ls
            . modelLState_dpstate
            . modelDPState_pstate
            . modelPState_poolParams
        )
      <*> pure dcertsWithoutRdmr

  refunds <-
    modelKeyRefunds
      <$> use (modelLedger . modelLedger_nes . to getModelPParams)
      <*> pure dcertsWithoutRdmr
  ins <- genInputs haveCollateral deposits refunds
  wdrl <- genWdrl haveCollateral

  mint <- case haveCollateral of
    NoPlutusSupport () -> pure mempty
    SupportsPlutus False -> pure mempty
    SupportsPlutus True -> genMint $ foldMap (_mtxo_value . snd) ins

  (outs, fee) <- genOutputs haveCollateral ins mint deposits refunds (foldMap fst wdrl)
  let txn =
        ModelTx
          { _mtxInputs = fmap fst ins,
            _mtxOutputs = outs,
            _mtxFee = fee,
            _mtxDCert = [],
            _mtxWdrl = wdrl,
            _mtxMint = mint,
            _mtxCollateral = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () Set.empty,
            _mtxValidity = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () (IsValid True),
            _mtxWitnessSigs = Set.empty
          }

  utxos <- use (modelLedger . modelLedger_nes . modelNewEpochState_es . modelEpochState_ls . modelLState_utxoSt . modelUTxOState_utxo)

  let nc = needCollateral ins wdrl mint dcerts
      txn' =
        txn
          { _mtxDCert = clobberDCertRedeemers dcerts,
            _mtxCollateral = if nc then collateral else _mtxCollateral txn
          }
  uses modelLedger $
    witnessModelTx
      txn'
        { _mtxValidity = mapSupportsFeature (const $ validateModelTx utxos txn') $ _mtxValidity txn'
        }

clobberDCertRedeemers ::
  forall era.
  [(ModelDCert era, ModelRedeemer (ScriptFeature era))] ->
  [(ModelDCert era, ModelRedeemer (ScriptFeature era))]
clobberDCertRedeemers dcerts = State.evalState (traverse go dcerts) Set.empty
  where
    go ::
      (ModelDCert era, ModelRedeemer (ScriptFeature era)) ->
      State.State
        (Set.Set (ModelDCert era))
        (ModelDCert era, ModelRedeemer (ScriptFeature era))
    go x@(dcert, rdmr) = case rdmr of
      SupportsPlutus (Just _) -> do
        oldRdmr <- at dcert <<.= Just ()
        pure (dcert, mapSupportsFeature (preview _Nothing oldRdmr *>) rdmr)
      _ -> pure x
