{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Model.Generators.Tx where

import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Keys (KeyRole (..))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.Slot (SlotNo)
import Control.Lens
  ( has,
    to,
    use,
    uses,
    (.=),
  )
import Control.Monad (replicateM)
import Control.Monad.Reader.Class (asks)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import QuickCheck.GenT
  ( liftGen,
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
    IfSupportsMint,
    IfSupportsPlutus (..),
    KnownScriptFeature,
    ScriptFeature,
    TyValueExpected (..),
    ValueFeature,
    fromSupportsMint,
    ifSupportsMint,
    ifSupportsPlutus,
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
    guardHaveCollateral,
  )
import Test.Cardano.Ledger.Model.Generators.TxOut
  ( genInputs,
    genOutputs,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential (..),
    ModelScript (..),
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
  ( ModelTxOut,
    ModelUTxOId,
    modelTxOut_address,
  )
import Test.Cardano.Ledger.Model.UTxO
  ( ModelUTxOMap (..),
  )

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

genModelTx :: forall era m st. SlotNo -> HasGenModelM st era m => m (ModelTx era)
genModelTx slot = do
  (haveCollateral, collateral) <- genCollateral
  ins <- genInputs haveCollateral
  wdrl <- fmap Val.inject <$> genWdrl haveCollateral

  mint <- pure $ ifSupportsMint (Proxy :: Proxy (ValueFeature era)) () mempty
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
