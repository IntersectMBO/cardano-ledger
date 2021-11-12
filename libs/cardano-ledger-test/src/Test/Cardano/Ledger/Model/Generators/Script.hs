{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Model.Generators.Script where

import Control.Lens (to, uses)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified PlutusTx (Data (..))
import QuickCheck.GenT
  ( Gen,
    choose,
  )
import Test.Cardano.Ledger.Model.API
  ( getModelLedger_utxos,
    modelLedger,
  )
import Test.Cardano.Ledger.Model.FeatureSet
  ( IfSupportsPlutus (..),
    KnownScriptFeature,
    ScriptFeature,
    ifSupportsPlutus,
    mapSupportsPlutus,
    reifySupportsPlutus,
    traverseSupportsPlutus,
  )
import Test.Cardano.Ledger.Model.Generators
  ( AllowScripts,
    HasGenModelM,
    chooseElems,
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelCredential (..),
  )
import Test.Cardano.Ledger.Model.TxOut
  ( ModelUTxOId,
  )
import Test.Cardano.Ledger.Model.UTxO (ModelUTxOMap (..))

genScriptData :: forall sf r. KnownScriptFeature sf => ModelCredential r sf -> Gen (IfSupportsPlutus () (Maybe PlutusTx.Data) sf)
genScriptData addr = traverseSupportsPlutus id $
  ifSupportsPlutus (Proxy :: Proxy sf) () $ case addr of
    ModelKeyHashObj _ -> pure Nothing
    -- ModelScriptHashObj _ -> Just . PlutusTx.I <$> arbitrary
    ModelScriptHashObj _ -> Just . PlutusTx.I <$> pure 0

genCollateral ::
  forall era m st.
  HasGenModelM st era m =>
  m (AllowScripts (ScriptFeature era), IfSupportsPlutus () (Set ModelUTxOId) (ScriptFeature era))
genCollateral = do
  res <- flip traverseSupportsPlutus (reifySupportsPlutus (Proxy :: Proxy (ScriptFeature era))) $ \() -> do
    availableCollateralInputs <- uses (modelLedger . to getModelLedger_utxos) $ _modelUTxOMap_collateralUtxos
    numCollateralInputs <- choose (1, min 5 (Set.size availableCollateralInputs - 1))
    (collateral, rest) <- chooseElems numCollateralInputs availableCollateralInputs
    -- avoid spending the last unlocked utxo
    pure $
      if 10 > Set.size rest -- genInputs may use up to 8 inputs.
        then Set.empty
        else collateral

  pure (mapSupportsPlutus (not . Set.null) res, res)

guardHaveCollateral ::
  IfSupportsPlutus () Bool sf ->
  ModelCredential k sf ->
  Maybe (ModelCredential k sf)
guardHaveCollateral (SupportsPlutus False) (ModelScriptHashObj _) = Nothing
guardHaveCollateral _ x = Just x
