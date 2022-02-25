{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Model.TxOut where

import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Val as Val
import Control.DeepSeq (NFData)
import Control.Lens
  ( Lens',
    has,
    lens,
  )
import Data.Functor.Identity (Identity (..))
import Data.Group (Group (..))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import qualified PlutusTx (Data (..))
import Quiet (Quiet (..))
import Test.Cardano.Ledger.Model.BaseTypes (ModelValue (..), filterModelValue)
import Test.Cardano.Ledger.Model.FeatureSet
  ( FeatureTag (..),
    IfSupportsMint (..),
    IfSupportsPlutus,
    KnownScriptFeature,
    RequiredFeatures (..),
    ScriptFeature,
    ValueFeature,
    bifoldMapSupportsFeature,
    filterSupportsPlutus,
    hasKnownValueFeature,
    ifSupportsPlutus,
  )
import Test.Cardano.Ledger.Model.PParams
  ( ModelPParams,
    ModelPParamsF (..),
  )
import Test.Cardano.Ledger.Model.Script
  ( ModelAddress (..),
    ModelCredential (..),
    filterModelAddress,
    modelAddress_pmt,
    _ModelKeyHashObj,
  )

newtype ModelUTxOId = ModelUTxOId {unModelUTxOId :: Integer}
  deriving (Eq, Ord, Num, Enum, Generic, NFData)

deriving newtype instance Show ModelUTxOId

data ModelTxOut era = ModelTxOut
  { _mtxo_address :: !(ModelAddress (ScriptFeature era)),
    _mtxo_value :: !(ModelValue (ValueFeature era) era),
    _mtxo_data :: !(IfSupportsPlutus () (Maybe PlutusTx.Data) (ScriptFeature era))
  }
  deriving (Eq, Ord, Generic)
  deriving (Show) via Quiet (ModelTxOut era)

instance NFData (ModelTxOut era)

instance RequiredFeatures ModelTxOut where
  filterFeatures tag@(FeatureTag v _) (ModelTxOut addr qty dat) =
    hasKnownValueFeature v $
      ModelTxOut
        <$> filterModelAddress tag addr
        <*> (filterFeatures tag =<< filterModelValue qty)
        <*> (filterSupportsPlutus tag dat)

-- | Convenience function to create a spendable ModelTxOut
modelTxOut :: forall era. KnownScriptFeature (ScriptFeature era) => ModelAddress (ScriptFeature era) -> ModelValue (ValueFeature era) era -> ModelTxOut era
modelTxOut a v = ModelTxOut a v dh
  where
    dh = ifSupportsPlutus (Proxy :: Proxy (ScriptFeature era)) () $
      case _modelAddress_pmt a of
        ModelKeyHashObj _ -> Nothing
        ModelScriptHashObj _ -> Just $ PlutusTx.I 42

modelTxOut_address :: forall era. Lens' (ModelTxOut era) (ModelAddress (ScriptFeature era))
modelTxOut_address = lens _mtxo_address (\s b -> s {_mtxo_address = b})
{-# INLINE modelTxOut_address #-}

modelTxOut_value :: Lens' (ModelTxOut era) (ModelValue (ValueFeature era) era)
modelTxOut_value = lens _mtxo_value (\s b -> s {_mtxo_value = b})
{-# INLINE modelTxOut_value #-}

modelTxOut_data :: Lens' (ModelTxOut era) (IfSupportsPlutus () (Maybe PlutusTx.Data) (ScriptFeature era))
modelTxOut_data = lens _mtxo_data (\s b -> s {_mtxo_data = b})
{-# INLINE modelTxOut_data #-}

modelMinUTxOCoins :: ModelPParams era -> ModelTxOut era -> Coin
modelMinUTxOCoins pp txout = case runIdentity $ _modelPParams_coinsPerUTxOWord pp of
  NoMintSupport x -> x
  SupportsMint coinsPerUTxOWord -> coinsPerUTxOWord `pow` modelUTxOEntrySize txout

-- SEE Fig 17 [GL-D2]
-- TODO: this is not at all correct, just a placeholder
modelUTxOEntrySize :: ModelTxOut era -> Integer
modelUTxOEntrySize (ModelTxOut _a v d) =
  utxoEntrySizeWithoutVal + Val.size v' + bifoldMapSupportsFeature (\() -> 0) modelDataHashSize d
  where
    v' = unModelValue v
    utxoEntrySizeWithoutVal = 29 -- according to spec, anways.
    modelDataHashSize = maybe 0 (const 10)

canBeUsedAsCollateral :: ModelTxOut era -> Bool
canBeUsedAsCollateral txo
  | not $ has (modelTxOut_address . modelAddress_pmt . _ModelKeyHashObj) txo = False
  | not $ Val.isAdaOnly $ _mtxo_value txo = False
  | otherwise = True
