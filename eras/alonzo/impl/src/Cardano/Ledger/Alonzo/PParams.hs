{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Alonzo.PParams
  ( -- * Era Agnostic
    AlonzoEraPParams,
    ppCoinsPerUTxOWordL,
    ppCostModelsL,
    ppPricesL,
    ppMaxTxExUnitsL,
    ppMaxBlockExUnitsL,
    ppMaxValSizeL,
    ppCollateralPercentageL,
    ppMaxCollateralInputsL,
    ppuCoinsPerUTxOWordL,
    ppuCostModelsL,
    ppuPricesL,
    ppuMaxTxExUnitsL,
    ppuMaxBlockExUnitsL,
    ppuMaxValSizeL,
    ppuCollateralPercentageL,
    ppuMaxCollateralInputsL,

    -- * Alonzo specific
    AlonzoPParams (..),
    UpgradeAlonzoPParams (..),
    DowngradeAlonzoPParams (..),
    emptyAlonzoPParams,
    emptyAlonzoPParamsUpdate,
    upgradeAlonzoPParams,
    downgradeAlonzoPParams,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    OrdExUnits(..),
  )
where

import Data.Default.Class (Default(def))
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Era (AlonzoEra)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    CostModels (..),
    ExUnits (..),
    Prices (..),
    getCostModelLanguage,
    getCostModelParams,
    zipSemiExUnits,
  )
import Cardano.Ledger.BaseTypes (EpochNo (..), NonNegativeInterval, Nonce (NeutralNonce), StrictMaybe (..), UnitInterval, isSNothing)
import qualified Cardano.Ledger.BaseTypes as BT (ProtVer (..))
import Cardano.Ledger.Binary
  ( Encoding,
    FromCBOR (..),
    FromCBORGroup (..),
    ToCBOR (..),
    ToCBORGroup (..),
    encodeFoldableAsDefLenList,
    encodeFoldableAsIndefLenList,
    encodeMapLen,
    encodeNull,
    encodePreEncoded,
    serialize',
    serializeEncoding',
  )
import Cardano.Ledger.Binary.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field (..),
    Wrapped (..),
    decode,
    encode,
    field,
    (!>),
    (<!),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.HKD (HKD, HKDFunctor (..))
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))
import Cardano.Ledger.Language (Language (..))
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Control.DeepSeq (NFData)
import Data.Aeson as Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.!=),
    (.:),
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, (^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | Protocol parameters.
-- Shelley parameters + additional ones
data AlonzoPParams f era = AlonzoPParams
  { -- | The linear factor for the minimum fee calculation
    appMinFeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    appMinFeeB :: !(HKD f Natural),
    -- | Maximal block body size
    appMaxBBSize :: !(HKD f Natural),
    -- | Maximal transaction size
    appMaxTxSize :: !(HKD f Natural),
    -- | Maximal block header size
    appMaxBHSize :: !(HKD f Natural),
    -- | The amount of a key registration deposit
    appKeyDeposit :: !(HKD f Coin),
    -- | The amount of a pool registration deposit
    appPoolDeposit :: !(HKD f Coin),
    -- | Maximum number of epochs in the future a pool retirement is allowed to
    -- be scheduled for.
    appEMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    appNOpt :: !(HKD f Natural),
    -- | Pool influence
    appA0 :: !(HKD f NonNegativeInterval),
    -- | Monetary expansion
    appRho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    appTau :: !(HKD f UnitInterval),
    -- | Decentralization parameter. Note that the scale is inverted here - a
    -- value of 0 indicates full decentralisation, where 1 indicates full
    -- federalisation.
    appD :: !(HKD f UnitInterval),
    -- | Extra entropy
    appExtraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    appProtocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum Stake Pool Cost
    appMinPoolCost :: !(HKD f Coin),
    -- new/updated for alonzo

    -- | Cost in lovelace per word (8 bytes) of UTxO storage (instead of appMinUTxOValue)
    appCoinsPerUTxOWord :: !(HKD f CoinPerWord),
    -- | Cost models for non-native script languages
    appCostModels :: !(HKD f CostModels),
    -- | Prices of execution units (for non-native script languages)
    appPrices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    appMaxTxExUnits :: !(HKD f OrdExUnits),
    -- | Max total script execution resources units allowed per block
    appMaxBlockExUnits :: !(HKD f OrdExUnits),
    -- | Max size of a Value in an output
    appMaxValSize :: !(HKD f Natural),
    -- | Percentage of the txfee which must be provided as collateral when
    -- including non-native scripts.
    appCollateralPercentage :: !(HKD f Natural),
    -- | Maximum number of collateral inputs allowed in a transaction
    appMaxCollateralInputs :: !(HKD f Natural)
  }
  deriving (Generic)

deriving instance Eq (AlonzoPParams Identity era)

deriving instance Ord (AlonzoPParams Identity era)

deriving instance Show (AlonzoPParams Identity era)

instance NoThunks (AlonzoPParams Identity era)

instance NFData (AlonzoPParams Identity era)

deriving instance Eq (AlonzoPParams StrictMaybe era)

deriving instance Ord (AlonzoPParams StrictMaybe era)

deriving instance Show (AlonzoPParams StrictMaybe era)

instance NoThunks (AlonzoPParams StrictMaybe era)

instance NFData (AlonzoPParams StrictMaybe era)

instance Crypto c => EraPParams (AlonzoEra c) where
  type PParamsHKD f (AlonzoEra c) = AlonzoPParams f (AlonzoEra c)
  type UpgradePParams f (AlonzoEra c) = UpgradeAlonzoPParams f
  type DowngradePParams f (AlonzoEra c) = DowngradeAlonzoPParams f

  emptyPParamsIdentity = emptyAlonzoPParams
  emptyPParamsStrictMaybe = emptyAlonzoPParamsUpdate

  upgradePParamsHKD = upgradeAlonzoPParams
  downgradePParamsHKD = downgradeAlonzoPParams

  hkdMinFeeAL = lens appMinFeeA $ \pp x -> pp {appMinFeeA = x}
  hkdMinFeeBL = lens appMinFeeB $ \pp x -> pp {appMinFeeB = x}
  hkdMaxBBSizeL = lens appMaxBBSize $ \pp x -> pp {appMaxBBSize = x}
  hkdMaxTxSizeL = lens appMaxTxSize $ \pp x -> pp {appMaxTxSize = x}
  hkdMaxBHSizeL = lens appMaxBHSize $ \pp x -> pp {appMaxBHSize = x}
  hkdKeyDepositL = lens appKeyDeposit $ \pp x -> pp {appKeyDeposit = x}
  hkdPoolDepositL = lens appPoolDeposit $ \pp x -> pp {appPoolDeposit = x}
  hkdEMaxL = lens appEMax $ \pp x -> pp {appEMax = x}
  hkdNOptL = lens appNOpt $ \pp x -> pp {appNOpt = x}
  hkdA0L = lens appA0 $ \pp x -> pp {appA0 = x}
  hkdRhoL = lens appRho $ \pp x -> pp {appRho = x}
  hkdTauL = lens appTau $ \pp x -> pp {appTau = x}
  hkdDL = lens appD $ \pp x -> pp {appD = x}
  hkdExtraEntropyL = lens appExtraEntropy $ \pp x -> pp {appExtraEntropy = x}
  hkdProtocolVersionL = lens appProtocolVersion $ \pp x -> pp {appProtocolVersion = x}
  hkdMinUTxOValueL = notSupportedInThisEraL
  hkdMinPoolCostL = lens appMinPoolCost $ \pp x -> pp {appMinPoolCost = x}

instance Crypto c => AlonzoEraPParams (AlonzoEra c) where
  hkdCoinsPerUTxOWordL = lens appCoinsPerUTxOWord $ \pp x -> pp {appCoinsPerUTxOWord = x}
  hkdCostModelsL = lens appCostModels $ \pp x -> pp {appCostModels = x}
  hkdPricesL = lens appPrices $ \pp x -> pp {appPrices = x}
  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (AlonzoEra c)) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . appMaxTxExUnits) $ \pp x ->
      pp {appMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (AlonzoEra c)) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . appMaxBlockExUnits) $ \pp x ->
      pp {appMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens appMaxValSize $ \pp x -> pp {appMaxValSize = x}
  hkdCollateralPercentageL =
    lens appCollateralPercentage $ \pp x -> pp {appCollateralPercentage = x}
  hkdMaxCollateralInputsL =
    lens appMaxCollateralInputs $ \pp x -> pp {appMaxCollateralInputs = x}

instance Era era => ToCBOR (AlonzoPParams Identity era) where
  toCBOR
    AlonzoPParams
      { appMinFeeA,
        appMinFeeB,
        appMaxBBSize,
        appMaxTxSize,
        appMaxBHSize,
        appKeyDeposit,
        appPoolDeposit,
        appEMax,
        appNOpt,
        appA0,
        appRho,
        appTau,
        appD,
        appExtraEntropy,
        appProtocolVersion,
        appMinPoolCost,
        appCoinsPerUTxOWord,
        appCostModels,
        appPrices,
        appMaxTxExUnits,
        appMaxBlockExUnits,
        appMaxValSize,
        appCollateralPercentage,
        appMaxCollateralInputs
      } =
      encode $
        Rec (AlonzoPParams @Identity)
          !> To appMinFeeA
          !> To appMinFeeB
          !> To appMaxBBSize
          !> To appMaxTxSize
          !> To appMaxBHSize
          !> To appKeyDeposit
          !> To appPoolDeposit
          !> To appEMax
          !> To appNOpt
          !> To appA0
          !> To appRho
          !> To appTau
          !> To appD
          !> To appExtraEntropy
          !> E toCBORGroup appProtocolVersion
          !> To appMinPoolCost
          -- new/updated for alonzo
          !> To appCoinsPerUTxOWord
          !> To appCostModels
          !> To appPrices
          !> To appMaxTxExUnits
          !> To appMaxBlockExUnits
          !> To appMaxValSize
          !> To appCollateralPercentage
          !> To appMaxCollateralInputs

instance Era era => FromCBOR (AlonzoPParams Identity era) where
  fromCBOR =
    decode $
      RecD AlonzoPParams
        <! From -- appMinFeeA
        <! From -- appMinFeeB
        <! From -- appMaxBBSize
        <! From -- appMaxTxSize
        <! From -- appMaxBHSize
        <! From -- appKeyDeposit
        <! From -- appPoolDeposit
        <! From -- appEMax
        <! From -- appNOpt
        <! From -- appA0
        <! From -- appRho
        <! From -- appTau
        <! From -- appD
        <! From -- appExtraEntropy
        <! D fromCBORGroup -- appProtocolVersion
        <! From -- appMinPoolCost
        -- new/updated for alonzo
        <! From -- appCoinsPerUTxOWord
        <! From -- appCostModels
        <! From -- appPrices
        <! From -- appMaxTxExUnits
        <! From -- appMaxBlockExUnits
        <! From -- appMaxValSize
        <! From -- appCollateralPercentage
        <! From -- appMaxCollateralInputs

instance ToJSON (AlonzoPParams Identity era) where
  toJSON AlonzoPParams {..} =
    Aeson.object
      [ "minFeeA" .= appMinFeeA,
        "minFeeB" .= appMinFeeB,
        "maxBlockBodySize" .= appMaxBBSize,
        "maxTxSize" .= appMaxTxSize,
        "maxBlockHeaderSize" .= appMaxBHSize,
        "keyDeposit" .= appKeyDeposit,
        "poolDeposit" .= appPoolDeposit,
        "eMax" .= appEMax,
        "nOpt" .= appNOpt,
        "a0" .= appA0,
        "rho" .= appRho,
        "tau" .= appTau,
        "decentralisationParam" .= appD,
        "extraEntropy" .= appExtraEntropy,
        "protocolVersion" .= appProtocolVersion,
        "minPoolCost" .= appMinPoolCost,
        "lovelacePerUTxOWord" .= appCoinsPerUTxOWord,
        "costmdls" .= appCostModels,
        "prices" .= appPrices,
        "maxTxExUnits" .= appMaxTxExUnits,
        "maxBlockExUnits" .= appMaxBlockExUnits,
        "maxValSize" .= appMaxValSize,
        "collateralPercentage" .= appCollateralPercentage,
        "maxCollateralInputs " .= appMaxCollateralInputs
      ]

instance FromJSON (AlonzoPParams Identity era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      AlonzoPParams
        <$> obj
        .: "minFeeA"
        <*> obj
        .: "minFeeB"
        <*> obj
        .: "maxBlockBodySize"
        <*> obj
        .: "maxTxSize"
        <*> obj
        .: "maxBlockHeaderSize"
        <*> obj
        .: "keyDeposit"
        <*> obj
        .: "poolDeposit"
        <*> obj
        .: "eMax"
        <*> obj
        .: "nOpt"
        <*> obj
        .: "a0"
        <*> obj
        .: "rho"
        <*> obj
        .: "tau"
        <*> obj
        .: "decentralisationParam"
        <*> obj
        .: "extraEntropy"
        <*> obj
        .: "protocolVersion"
        <*> obj
        .: "minPoolCost"
        .!= mempty
        <*> obj
        .: "lovelacePerUTxOWord"
        <*> obj
        .: "costmdls"
        <*> obj
        .: "prices"
        <*> obj
        .: "maxTxExUnits"
        <*> obj
        .: "maxBlockExUnits"
        <*> obj
        .: "maxValSize"
        <*> obj
        .: "collateralPercentage"
        <*> obj
        .: "maxCollateralInputs"

-- | This is a helper type that allows us to define an `Ord` instance for executions units
-- without affecting the `ExUnits` type. This is needed in order to derive an `Ord` instance`
-- for PParams. This is just a helper type and should not be used directly. Both lenses
-- that operate on TxExUnits and BlockExUnits use the `ExUnits` type, not this one.
newtype OrdExUnits = OrdExUnits {unOrdExUnits :: ExUnits}
  deriving (Eq)
  deriving newtype (Show, NoThunks, NFData, FromCBOR, ToCBOR, FromJSON, ToJSON, ToExpr)

instance Ord OrdExUnits where
  compare = coerce (zipSemiExUnits compare)

-- | Parameters that were added in Alonzo
data UpgradeAlonzoPParams f = UpgradeAlonzoPParams
  { uappCoinsPerUTxOWord :: !(HKD f CoinPerWord),
    uappCostModels :: !(HKD f CostModels),
    uappPrices :: !(HKD f Prices),
    uappMaxTxExUnits :: !(HKD f ExUnits),
    uappMaxBlockExUnits :: !(HKD f ExUnits),
    uappMaxValSize :: !(HKD f Natural),
    uappCollateralPercentage :: !(HKD f Natural),
    uappMaxCollateralInputs :: !(HKD f Natural)
  }
  deriving (Generic)

deriving instance Eq (UpgradeAlonzoPParams Identity)

deriving instance Show (UpgradeAlonzoPParams Identity)

instance NoThunks (UpgradeAlonzoPParams Identity)

instance NFData (UpgradeAlonzoPParams Identity)

instance Default (UpgradeAlonzoPParams StrictMaybe) where
  def =
    UpgradeAlonzoPParams
      { uappCoinsPerUTxOWord = SNothing,
        uappCostModels = SNothing,
        uappPrices = SNothing,
        uappMaxTxExUnits = SNothing,
        uappMaxBlockExUnits = SNothing,
        uappMaxValSize = SNothing,
        uappCollateralPercentage = SNothing,
        uappMaxCollateralInputs = SNothing
      }

-- | Parameters that were removed in Alonzo
newtype DowngradeAlonzoPParams f = DowngradeAlonzoPParams
  { dappMinUTxOValue :: HKD f Coin
  }
  deriving (Generic)

deriving instance Eq (DowngradeAlonzoPParams Identity)

deriving instance Show (DowngradeAlonzoPParams Identity)

instance NoThunks (DowngradeAlonzoPParams Identity)

instance NFData (DowngradeAlonzoPParams Identity)

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyAlonzoPParams :: forall era. Era era => AlonzoPParams Identity era
emptyAlonzoPParams =
  AlonzoPParams
    { appMinFeeA = 0,
      appMinFeeB = 0,
      appMaxBBSize = 0,
      appMaxTxSize = 2048,
      appMaxBHSize = 0,
      appKeyDeposit = Coin 0,
      appPoolDeposit = Coin 0,
      appEMax = EpochNo 0,
      appNOpt = 100,
      appA0 = minBound,
      appRho = minBound,
      appTau = minBound,
      appD = minBound,
      appExtraEntropy = NeutralNonce,
      appProtocolVersion = BT.ProtVer (eraProtVerLow @era) 0,
      appMinPoolCost = mempty,
      -- new/updated for alonzo
      appCoinsPerUTxOWord = CoinPerWord (Coin 0),
      appCostModels = CostModels mempty,
      appPrices = Prices minBound minBound,
      appMaxTxExUnits = OrdExUnits $ ExUnits 0 0,
      appMaxBlockExUnits = OrdExUnits $ ExUnits 0 0,
      appMaxValSize = 0,
      appCollateralPercentage = 150,
      appMaxCollateralInputs = 5
    }

emptyAlonzoPParamsUpdate :: AlonzoPParams StrictMaybe era
emptyAlonzoPParamsUpdate =
  AlonzoPParams
    { appMinFeeA = SNothing,
      appMinFeeB = SNothing,
      appMaxBBSize = SNothing,
      appMaxTxSize = SNothing,
      appMaxBHSize = SNothing,
      appKeyDeposit = SNothing,
      appPoolDeposit = SNothing,
      appEMax = SNothing,
      appNOpt = SNothing,
      appA0 = SNothing,
      appRho = SNothing,
      appTau = SNothing,
      appD = SNothing,
      appExtraEntropy = SNothing,
      appProtocolVersion = SNothing,
      appMinPoolCost = SNothing,
      -- new/updated for alonzo
      appCoinsPerUTxOWord = SNothing,
      appCostModels = SNothing,
      appPrices = SNothing,
      appMaxTxExUnits = SNothing,
      appMaxBlockExUnits = SNothing,
      appMaxValSize = SNothing,
      appCollateralPercentage = SNothing,
      appMaxCollateralInputs = SNothing
    }

-- =======================================================
-- A PParamsUpdate has StrictMaybe fields, we want to Sparse encode it, by
-- writing only those fields where the field is (SJust x), that is the role of
-- the local function (omitStrictMaybe key x)

encodePParamsUpdate ::
  AlonzoPParams StrictMaybe era ->
  Encode ('Closed 'Sparse) (AlonzoPParams StrictMaybe era)
encodePParamsUpdate ppup =
  Keyed AlonzoPParams
    !> omitStrictMaybe 0 (appMinFeeA ppup) toCBOR
    !> omitStrictMaybe 1 (appMinFeeB ppup) toCBOR
    !> omitStrictMaybe 2 (appMaxBBSize ppup) toCBOR
    !> omitStrictMaybe 3 (appMaxTxSize ppup) toCBOR
    !> omitStrictMaybe 4 (appMaxBHSize ppup) toCBOR
    !> omitStrictMaybe 5 (appKeyDeposit ppup) toCBOR
    !> omitStrictMaybe 6 (appPoolDeposit ppup) toCBOR
    !> omitStrictMaybe 7 (appEMax ppup) toCBOR
    !> omitStrictMaybe 8 (appNOpt ppup) toCBOR
    !> omitStrictMaybe 9 (appA0 ppup) toCBOR
    !> omitStrictMaybe 10 (appRho ppup) toCBOR
    !> omitStrictMaybe 11 (appTau ppup) toCBOR
    !> omitStrictMaybe 12 (appD ppup) toCBOR
    !> omitStrictMaybe 13 (appExtraEntropy ppup) toCBOR
    !> omitStrictMaybe 14 (appProtocolVersion ppup) toCBOR
    !> omitStrictMaybe 16 (appMinPoolCost ppup) toCBOR
    !> omitStrictMaybe 17 (appCoinsPerUTxOWord ppup) toCBOR
    !> omitStrictMaybe 18 (appCostModels ppup) toCBOR
    !> omitStrictMaybe 19 (appPrices ppup) toCBOR
    !> omitStrictMaybe 20 (appMaxTxExUnits ppup) toCBOR
    !> omitStrictMaybe 21 (appMaxBlockExUnits ppup) toCBOR
    !> omitStrictMaybe 22 (appMaxValSize ppup) toCBOR
    !> omitStrictMaybe 23 (appCollateralPercentage ppup) toCBOR
    !> omitStrictMaybe 24 (appMaxCollateralInputs ppup) toCBOR
  where
    omitStrictMaybe ::
      Word -> StrictMaybe a -> (a -> Encoding) -> Encode ('Closed 'Sparse) (StrictMaybe a)
    omitStrictMaybe key x enc = Omit isSNothing (Key key (E (enc . fromSJust) x))

    fromSJust :: StrictMaybe a -> a
    fromSJust (SJust x) = x
    fromSJust SNothing = error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing."

instance Era era => ToCBOR (AlonzoPParams StrictMaybe era) where
  toCBOR ppup = encode (encodePParamsUpdate ppup)

updateField :: Word -> Field (AlonzoPParams StrictMaybe era)
updateField 0 = field (\x up -> up {appMinFeeA = SJust x}) From
updateField 1 = field (\x up -> up {appMinFeeB = SJust x}) From
updateField 2 = field (\x up -> up {appMaxBBSize = SJust x}) From
updateField 3 = field (\x up -> up {appMaxTxSize = SJust x}) From
updateField 4 = field (\x up -> up {appMaxBHSize = SJust x}) From
updateField 5 = field (\x up -> up {appKeyDeposit = SJust x}) From
updateField 6 = field (\x up -> up {appPoolDeposit = SJust x}) From
updateField 7 = field (\x up -> up {appEMax = SJust x}) From
updateField 8 = field (\x up -> up {appNOpt = SJust x}) From
updateField 9 = field (\x up -> up {appA0 = SJust x}) From
updateField 10 = field (\x up -> up {appRho = SJust x}) From
updateField 11 = field (\x up -> up {appTau = SJust x}) From
updateField 12 = field (\x up -> up {appD = SJust x}) From
updateField 13 = field (\x up -> up {appExtraEntropy = SJust x}) From
updateField 14 = field (\x up -> up {appProtocolVersion = SJust x}) From
updateField 16 = field (\x up -> up {appMinPoolCost = SJust x}) From
updateField 17 = field (\x up -> up {appCoinsPerUTxOWord = SJust x}) From
updateField 18 = field (\x up -> up {appCostModels = SJust x}) From
updateField 19 = field (\x up -> up {appPrices = SJust x}) From
updateField 20 = field (\x up -> up {appMaxTxExUnits = SJust x}) From
updateField 21 = field (\x up -> up {appMaxBlockExUnits = SJust x}) From
updateField 22 = field (\x up -> up {appMaxValSize = SJust x}) From
updateField 23 = field (\x up -> up {appCollateralPercentage = SJust x}) From
updateField 24 = field (\x up -> up {appMaxCollateralInputs = SJust x}) From
updateField k = field (\_x up -> up) (Invalid k)

instance Era era => FromCBOR (AlonzoPParams StrictMaybe era) where
  fromCBOR =
    decode (SparseKeyed "PParamsUpdate" emptyAlonzoPParamsUpdate updateField [])

-- ===================================================
-- Figure 1: "Definitions Used in Protocol Parameters"

-- The LangDepView is a key value pair. The key is the (canonically) encoded
-- language tag and the value is the (canonically) encoded set of relevant
-- protocol parameters
data LangDepView = LangDepView {tag :: ByteString, params :: ByteString}
  deriving (Eq, Show, Ord, Generic, NoThunks)

encodeCostModel :: CostModel -> Encoding
encodeCostModel cm =
  case getCostModelLanguage cm of
    -- In the Alonzo era, the map of languages to cost models was mistakenly encoded
    -- using an indefinite CBOR map (contrary to canonical CBOR, as intended) when
    -- computing the script integrity hash.
    -- For this reason, PlutusV1 remains with this encoding.
    -- Future versions of Plutus, starting with PlutusV2 in the Babbage era, will
    -- use the intended definite length encoding.
    PlutusV1 -> encodeFoldableAsIndefLenList toCBOR $ getCostModelParams cm
    -- Since cost model serializations need to be independently reproduced,
    -- we use the 'canonical' serialization with definite list length.
    PlutusV2 -> encodeFoldableAsDefLenList toCBOR $ getCostModelParams cm

getLanguageView ::
  AlonzoEraPParams era =>
  PParams era ->
  Language ->
  LangDepView
getLanguageView pp lang =
  case lang of
    PlutusV1 ->
      LangDepView -- The silly double bagging is to keep compatibility with a past bug
        (serialize' version (serialize' version lang))
        (serialize' version costModelEncoding)
    PlutusV2 ->
      LangDepView
        (serialize' version lang)
        costModelEncoding
  where
    costModel = Map.lookup lang (unCostModels $ pp ^. ppCostModelsL)
    costModelEncoding = serializeEncoding' version $ maybe encodeNull encodeCostModel costModel
    version = BT.pvMajor $ pp ^. ppProtocolVersionL

encodeLangViews :: Set LangDepView -> Encoding
encodeLangViews views = encodeMapLen n <> foldMap encPair ascending
  where
    n = fromIntegral (Set.size views) :: Word
    ascending = sortBy (shortLex `on` tag) $ Set.toList views
    encPair (LangDepView k v) = encodePreEncoded k <> encodePreEncoded v
    shortLex :: ByteString -> ByteString -> Ordering
    shortLex a b
      | BS.length a < BS.length b = LT
      | BS.length a > BS.length b = GT
      | otherwise = compare a b

-- | Given the missing pieces Turn a ShelleyPParams into a AlonzoPParams
upgradeAlonzoPParams ::
  forall f era1 era2.
  HKDFunctor f =>
  UpgradeAlonzoPParams f ->
  ShelleyPParams f era1 ->
  AlonzoPParams f era2
upgradeAlonzoPParams UpgradeAlonzoPParams {..} ShelleyPParams {..} =
  AlonzoPParams
    { appMinFeeA = sppMinFeeA,
      appMinFeeB = sppMinFeeB,
      appMaxBBSize = sppMaxBBSize,
      appMaxTxSize = sppMaxTxSize,
      appMaxBHSize = sppMaxBHSize,
      appKeyDeposit = sppKeyDeposit,
      appPoolDeposit = sppPoolDeposit,
      appEMax = sppEMax,
      appNOpt = sppNOpt,
      appA0 = sppA0,
      appRho = sppRho,
      appTau = sppTau,
      appD = sppD,
      appExtraEntropy = sppExtraEntropy,
      appProtocolVersion = sppProtocolVersion,
      appMinPoolCost = sppMinPoolCost,
      -- new in alonzo
      appCoinsPerUTxOWord = uappCoinsPerUTxOWord,
      appCostModels = uappCostModels,
      appPrices = uappPrices,
      appMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits uappMaxTxExUnits,
      appMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits uappMaxBlockExUnits,
      appMaxValSize = uappMaxValSize,
      appCollateralPercentage = uappCollateralPercentage,
      appMaxCollateralInputs = uappMaxCollateralInputs
    }

-- | Turn an AlonzoPParams into a ShelleyParams
downgradeAlonzoPParams :: DowngradeAlonzoPParams f -> AlonzoPParams f era2 -> ShelleyPParams f era1
downgradeAlonzoPParams DowngradeAlonzoPParams {dappMinUTxOValue} AlonzoPParams {..} =
  ShelleyPParams
    { sppMinFeeA = appMinFeeA,
      sppMinFeeB = appMinFeeB,
      sppMaxBBSize = appMaxBBSize,
      sppMaxTxSize = appMaxTxSize,
      sppMaxBHSize = appMaxBHSize,
      sppKeyDeposit = appKeyDeposit,
      sppPoolDeposit = appPoolDeposit,
      sppEMax = appEMax,
      sppNOpt = appNOpt,
      sppA0 = appA0,
      sppRho = appRho,
      sppTau = appTau,
      sppD = appD,
      sppExtraEntropy = appExtraEntropy,
      sppProtocolVersion = appProtocolVersion,
      sppMinUTxOValue = dappMinUTxOValue, -- <- parameter that was dropped in Alonzo
      sppMinPoolCost = appMinPoolCost
    }

instance ToExpr (AlonzoPParams StrictMaybe era)

instance ToExpr (AlonzoPParams Identity era)

