{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Babbage.PParams
  ( BabbagePParams (..),
    emptyBabbagePParams,
    emptyBabbagePParamsUpdate,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    coinsPerUTxOWordToCoinsPerUTxOByte,
    coinsPerUTxOByteToCoinsPerUTxOWord,
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.PParams
  ( AlonzoPParams (..),
    LangDepView (..),
    OrdExUnits (..),
    encodeLangViews,
    getLanguageView,
  )
import Cardano.Ledger.Alonzo.Scripts
  ( CostModels (..),
    ExUnits (..),
    Prices (..),
  )
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    Nonce,
    ProtVer (..),
    StrictMaybe (..),
    UnitInterval,
    isSNothing,
  )
import Cardano.Ledger.Binary
  ( Encoding,
    FromCBOR (..),
    FromCBORGroup (..),
    ToCBOR (..),
    ToCBORGroup (..),
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
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.TreeDiff (ToExpr (..))
import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens, to)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | Babbage Protocol parameters. Ways in which parameters have changed from Alonzo: lack
-- of @d@, @extraEntropy@ and replacement of @coinsPerUTxOWord@ with @coinsPerUTxOByte@
data BabbagePParams f era = BabbagePParams
  { -- | The linear factor for the minimum fee calculation
    bppMinFeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    bppMinFeeB :: !(HKD f Natural),
    -- | Maximal block body size
    bppMaxBBSize :: !(HKD f Natural),
    -- | Maximal transaction size
    bppMaxTxSize :: !(HKD f Natural),
    -- | Maximal block header size
    bppMaxBHSize :: !(HKD f Natural),
    -- | The amount of a key registration deposit
    bppKeyDeposit :: !(HKD f Coin),
    -- | The amount of a pool registration deposit
    bppPoolDeposit :: !(HKD f Coin),
    -- | Maximum number of epochs in the future a pool retirement is allowed to
    -- be scheduled for.
    bppEMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    bppNOpt :: !(HKD f Natural),
    -- | Pool influence
    bppA0 :: !(HKD f NonNegativeInterval),
    -- | Monetary expansion
    bppRho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    bppTau :: !(HKD f UnitInterval),
    -- | Protocol version
    bppProtocolVersion :: !(HKD f ProtVer),
    -- | Minimum Stake Pool Cost
    bppMinPoolCost :: !(HKD f Coin),
    -- | Cost in lovelace per byte of UTxO storage (instead of bppCoinsPerUTxOByte)
    bppCoinsPerUTxOByte :: !(HKD f CoinPerByte),
    -- | Cost models for non-native script languages
    bppCostModels :: !(HKD f CostModels),
    -- | Prices of execution units (for non-native script languages)
    bppPrices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    bppMaxTxExUnits :: !(HKD f OrdExUnits),
    -- | Max total script execution resources units allowed per block
    bppMaxBlockExUnits :: !(HKD f OrdExUnits),
    -- | Max size of a Value in an output
    bppMaxValSize :: !(HKD f Natural),
    -- | Percentage of the txfee which must be provided as collateral when
    -- including non-native scripts.
    bppCollateralPercentage :: !(HKD f Natural),
    -- | Maximum number of collateral inputs allowed in a transaction
    bppMaxCollateralInputs :: !(HKD f Natural)
  }
  deriving (Generic)

deriving instance Eq (BabbagePParams Identity era)

deriving instance Ord (BabbagePParams Identity era)

deriving instance Show (BabbagePParams Identity era)

instance NoThunks (BabbagePParams Identity era)

instance NFData (BabbagePParams Identity era)

deriving instance Eq (BabbagePParams StrictMaybe era)

deriving instance Ord (BabbagePParams StrictMaybe era)

deriving instance Show (BabbagePParams StrictMaybe era)

instance NoThunks (BabbagePParams StrictMaybe era)

instance NFData (BabbagePParams StrictMaybe era)

data DowngradeBabbagePParams f = DowngradeBabbagePParams
  { dbppD :: !(HKD f UnitInterval),
    dbppExtraEntropy :: !(HKD f Nonce)
  }

instance Crypto c => EraPParams (BabbageEra c) where
  type PParamsHKD f (BabbageEra c) = BabbagePParams f (BabbageEra c)
  type UpgradePParams f (BabbageEra c) = ()
  type DowngradePParams f (BabbageEra c) = DowngradeBabbagePParams f

  emptyPParamsIdentity = emptyBabbagePParams
  emptyPParamsStrictMaybe = emptyBabbagePParamsUpdate

  upgradePParamsHKD () = upgradeBabbagePParams
  downgradePParamsHKD = downgradeBabbagePParams

  hkdMinFeeAL = lens bppMinFeeA $ \pp x -> pp {bppMinFeeA = x}
  hkdMinFeeBL = lens bppMinFeeB $ \pp x -> pp {bppMinFeeB = x}
  hkdMaxBBSizeL = lens bppMaxBBSize $ \pp x -> pp {bppMaxBBSize = x}
  hkdMaxTxSizeL = lens bppMaxTxSize $ \pp x -> pp {bppMaxTxSize = x}
  hkdMaxBHSizeL = lens bppMaxBHSize $ \pp x -> pp {bppMaxBHSize = x}
  hkdKeyDepositL = lens bppKeyDeposit $ \pp x -> pp {bppKeyDeposit = x}
  hkdPoolDepositL = lens bppPoolDeposit $ \pp x -> pp {bppPoolDeposit = x}
  hkdEMaxL = lens bppEMax $ \pp x -> pp {bppEMax = x}
  hkdNOptL = lens bppNOpt $ \pp x -> pp {bppNOpt = x}
  hkdA0L = lens bppA0 $ \pp x -> pp {bppA0 = x}
  hkdRhoL = lens bppRho $ \pp x -> pp {bppRho = x}
  hkdTauL = lens bppTau $ \pp x -> pp {bppTau = x}
  hkdProtocolVersionL = lens bppProtocolVersion $ \pp x -> pp {bppProtocolVersion = x}
  hkdMinPoolCostL = lens bppMinPoolCost $ \pp x -> pp {bppMinPoolCost = x}

  ppDG = to (const minBound)
  hkdDL = notSupportedInThisEraL
  hkdExtraEntropyL = notSupportedInThisEraL
  hkdMinUTxOValueL = notSupportedInThisEraL

instance Crypto c => AlonzoEraPParams (BabbageEra c) where
  hkdCoinsPerUTxOWordL = notSupportedInThisEraL
  hkdCostModelsL = lens bppCostModels $ \pp x -> pp {bppCostModels = x}
  hkdPricesL = lens bppPrices $ \pp x -> pp {bppPrices = x}
  hkdMaxTxExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (BabbageEra c)) (HKD f ExUnits)
  hkdMaxTxExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . bppMaxTxExUnits) $ \pp x ->
      pp {bppMaxTxExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxBlockExUnitsL :: forall f. HKDFunctor f => Lens' (PParamsHKD f (BabbageEra c)) (HKD f ExUnits)
  hkdMaxBlockExUnitsL =
    lens (hkdMap (Proxy @f) unOrdExUnits . bppMaxBlockExUnits) $ \pp x ->
      pp {bppMaxBlockExUnits = hkdMap (Proxy @f) OrdExUnits x}
  hkdMaxValSizeL = lens bppMaxValSize $ \pp x -> pp {bppMaxValSize = x}
  hkdCollateralPercentageL =
    lens bppCollateralPercentage $ \pp x -> pp {bppCollateralPercentage = x}
  hkdMaxCollateralInputsL =
    lens bppMaxCollateralInputs $ \pp x -> pp {bppMaxCollateralInputs = x}

instance Crypto c => BabbageEraPParams (BabbageEra c) where
  hkdCoinsPerUTxOByteL = lens bppCoinsPerUTxOByte (\pp x -> pp {bppCoinsPerUTxOByte = x})

instance Era era => ToCBOR (BabbagePParams Identity era) where
  toCBOR BabbagePParams {..} =
    encode
      ( Rec (BabbagePParams @Identity)
          !> To bppMinFeeA
          !> To bppMinFeeB
          !> To bppMaxBBSize
          !> To bppMaxTxSize
          !> To bppMaxBHSize
          !> To bppKeyDeposit
          !> To bppPoolDeposit
          !> To bppEMax
          !> To bppNOpt
          !> To bppA0
          !> To bppRho
          !> To bppTau
          !> E toCBORGroup bppProtocolVersion
          !> To bppMinPoolCost
          !> To bppCoinsPerUTxOByte
          !> To bppCostModels
          !> To bppPrices
          !> To bppMaxTxExUnits
          !> To bppMaxBlockExUnits
          !> To bppMaxValSize
          !> To bppCollateralPercentage
          !> To bppMaxCollateralInputs
      )

instance Era era => FromCBOR (BabbagePParams Identity era) where
  fromCBOR =
    decode $
      RecD BabbagePParams
        <! From -- bppMinFeeA
        <! From -- bppMinFeeB
        <! From -- bppMaxBBSize
        <! From -- bppMaxTxSize
        <! From -- bppMaxBHSize
        <! From -- bppKeyDeposit
        <! From -- bppPoolDeposit
        <! From -- bppEMax
        <! From -- bppNOpt
        <! From -- bppA0
        <! From -- bppRho
        <! From -- bppTau
        <! D fromCBORGroup -- bppProtocolVersion
        <! From -- bppMinPoolCost
        <! From -- bppCoinsPerUTxOByte
        <! From -- bppCostModels
        <! From -- bppPrices
        <! From -- bppMaxTxExUnits
        <! From -- bppMaxBlockExUnits
        <! From -- maxValSize
        <! From -- collateralPercentage
        <! From -- maxCollateralInputs

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyBabbagePParams :: forall era. Era era => BabbagePParams Identity era
emptyBabbagePParams =
  BabbagePParams
    { bppMinFeeA = 0,
      bppMinFeeB = 0,
      bppMaxBBSize = 0,
      bppMaxTxSize = 2048,
      bppMaxBHSize = 0,
      bppKeyDeposit = Coin 0,
      bppPoolDeposit = Coin 0,
      bppEMax = EpochNo 0,
      bppNOpt = 100,
      bppA0 = minBound,
      bppRho = minBound,
      bppTau = minBound,
      bppProtocolVersion = ProtVer (eraProtVerLow @era) 0,
      bppMinPoolCost = mempty,
      bppCoinsPerUTxOByte = CoinPerByte $ Coin 0,
      bppCostModels = CostModels mempty,
      bppPrices = Prices minBound minBound,
      bppMaxTxExUnits = OrdExUnits $ ExUnits 0 0,
      bppMaxBlockExUnits = OrdExUnits $ ExUnits 0 0,
      bppMaxValSize = 0,
      bppCollateralPercentage = 150,
      bppMaxCollateralInputs = 5
    }

emptyBabbagePParamsUpdate :: BabbagePParams StrictMaybe era
emptyBabbagePParamsUpdate =
  BabbagePParams
    { bppMinFeeA = SNothing,
      bppMinFeeB = SNothing,
      bppMaxBBSize = SNothing,
      bppMaxTxSize = SNothing,
      bppMaxBHSize = SNothing,
      bppKeyDeposit = SNothing,
      bppPoolDeposit = SNothing,
      bppEMax = SNothing,
      bppNOpt = SNothing,
      bppA0 = SNothing,
      bppRho = SNothing,
      bppTau = SNothing,
      bppProtocolVersion = SNothing,
      bppMinPoolCost = SNothing,
      bppCoinsPerUTxOByte = SNothing,
      bppCostModels = SNothing,
      bppPrices = SNothing,
      bppMaxTxExUnits = SNothing,
      bppMaxBlockExUnits = SNothing,
      bppMaxValSize = SNothing,
      bppCollateralPercentage = SNothing,
      bppMaxCollateralInputs = SNothing
    }

-- =======================================================
-- A PParamsUpdate has StrictMaybe fields, we want to Sparse encode it, by
-- writing only those fields where the field is (SJust x), that is the role of
-- the local function (omitStrictMaybe key x)

encodePParamsUpdate ::
  BabbagePParams StrictMaybe era ->
  Encode ('Closed 'Sparse) (BabbagePParams StrictMaybe era)
encodePParamsUpdate ppup =
  Keyed BabbagePParams
    !> omitStrictMaybe 0 (bppMinFeeA ppup) toCBOR
    !> omitStrictMaybe 1 (bppMinFeeB ppup) toCBOR
    !> omitStrictMaybe 2 (bppMaxBBSize ppup) toCBOR
    !> omitStrictMaybe 3 (bppMaxTxSize ppup) toCBOR
    !> omitStrictMaybe 4 (bppMaxBHSize ppup) toCBOR
    !> omitStrictMaybe 5 (bppKeyDeposit ppup) toCBOR
    !> omitStrictMaybe 6 (bppPoolDeposit ppup) toCBOR
    !> omitStrictMaybe 7 (bppEMax ppup) toCBOR
    !> omitStrictMaybe 8 (bppNOpt ppup) toCBOR
    !> omitStrictMaybe 9 (bppA0 ppup) toCBOR
    !> omitStrictMaybe 10 (bppRho ppup) toCBOR
    !> omitStrictMaybe 11 (bppTau ppup) toCBOR
    !> omitStrictMaybe 14 (bppProtocolVersion ppup) toCBOR
    !> omitStrictMaybe 16 (bppMinPoolCost ppup) toCBOR
    !> omitStrictMaybe 17 (bppCoinsPerUTxOByte ppup) toCBOR
    !> omitStrictMaybe 18 (bppCostModels ppup) toCBOR
    !> omitStrictMaybe 19 (bppPrices ppup) toCBOR
    !> omitStrictMaybe 20 (bppMaxTxExUnits ppup) toCBOR
    !> omitStrictMaybe 21 (bppMaxBlockExUnits ppup) toCBOR
    !> omitStrictMaybe 22 (bppMaxValSize ppup) toCBOR
    !> omitStrictMaybe 23 (bppCollateralPercentage ppup) toCBOR
    !> omitStrictMaybe 24 (bppMaxCollateralInputs ppup) toCBOR
  where
    omitStrictMaybe ::
      Word -> StrictMaybe a -> (a -> Encoding) -> Encode ('Closed 'Sparse) (StrictMaybe a)
    omitStrictMaybe key x enc = Omit isSNothing (Key key (E (enc . fromSJust) x))

    fromSJust :: StrictMaybe a -> a
    fromSJust (SJust x) = x
    fromSJust SNothing = error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing."

instance Era era => ToCBOR (BabbagePParams StrictMaybe era) where
  toCBOR ppup = encode (encodePParamsUpdate ppup)

updateField :: Word -> Field (BabbagePParams StrictMaybe era)
updateField 0 = field (\x up -> up {bppMinFeeA = SJust x}) From
updateField 1 = field (\x up -> up {bppMinFeeB = SJust x}) From
updateField 2 = field (\x up -> up {bppMaxBBSize = SJust x}) From
updateField 3 = field (\x up -> up {bppMaxTxSize = SJust x}) From
updateField 4 = field (\x up -> up {bppMaxBHSize = SJust x}) From
updateField 5 = field (\x up -> up {bppKeyDeposit = SJust x}) From
updateField 6 = field (\x up -> up {bppPoolDeposit = SJust x}) From
updateField 7 = field (\x up -> up {bppEMax = SJust x}) From
updateField 8 = field (\x up -> up {bppNOpt = SJust x}) From
updateField 9 = field (\x up -> up {bppA0 = SJust x}) From
updateField 10 = field (\x up -> up {bppRho = SJust x}) From
updateField 11 = field (\x up -> up {bppTau = SJust x}) From
updateField 14 = field (\x up -> up {bppProtocolVersion = SJust x}) From
updateField 16 = field (\x up -> up {bppMinPoolCost = SJust x}) From
updateField 17 = field (\x up -> up {bppCoinsPerUTxOByte = SJust x}) From
updateField 18 = field (\x up -> up {bppCostModels = SJust x}) From
updateField 19 = field (\x up -> up {bppPrices = SJust x}) From
updateField 20 = field (\x up -> up {bppMaxTxExUnits = SJust x}) From
updateField 21 = field (\x up -> up {bppMaxBlockExUnits = SJust x}) From
updateField 22 = field (\x up -> up {bppMaxValSize = SJust x}) From
updateField 23 = field (\x up -> up {bppCollateralPercentage = SJust x}) From
updateField 24 = field (\x up -> up {bppMaxCollateralInputs = SJust x}) From
updateField k = field (\_x up -> up) (Invalid k)

instance Era era => FromCBOR (BabbagePParams StrictMaybe era) where
  fromCBOR =
    decode
      (SparseKeyed "PParamsUpdate" emptyBabbagePParamsUpdate updateField [])

upgradeBabbagePParams ::
  PParamsHKD f (AlonzoEra c) ->
  BabbagePParams f (BabbageEra c)
upgradeBabbagePParams = undefined

downgradeBabbagePParams ::
  forall f c.
  HKDFunctor f =>
  DowngradeBabbagePParams f ->
  BabbagePParams f (BabbageEra c) ->
  PParamsHKD f (AlonzoEra c)
downgradeBabbagePParams DowngradeBabbagePParams {..} BabbagePParams {..} =
  AlonzoPParams
    { appMinFeeA = bppMinFeeA,
      appMinFeeB = bppMinFeeB,
      appMaxBBSize = bppMaxBBSize,
      appMaxTxSize = bppMaxTxSize,
      appMaxBHSize = bppMaxBHSize,
      appKeyDeposit = bppKeyDeposit,
      appPoolDeposit = bppPoolDeposit,
      appEMax = bppEMax,
      appNOpt = bppNOpt,
      appA0 = bppA0,
      appRho = bppRho,
      appTau = bppTau,
      appD = dbppD,
      appExtraEntropy = dbppExtraEntropy,
      appProtocolVersion = bppProtocolVersion,
      appMinPoolCost = bppMinPoolCost,
      appCoinsPerUTxOWord = hkdMap (Proxy @f) coinsPerUTxOByteToCoinsPerUTxOWord bppCoinsPerUTxOByte,
      appCostModels = bppCostModels,
      appPrices = bppPrices,
      appMaxTxExUnits = bppMaxTxExUnits,
      appMaxBlockExUnits = bppMaxBlockExUnits,
      appMaxValSize = bppMaxValSize,
      appCollateralPercentage = bppCollateralPercentage,
      appMaxCollateralInputs = bppMaxCollateralInputs
    }

-- | A word is 8 bytes, so to convert from coinsPerUTxOWord to coinsPerUTxOByte, rounding down.
coinsPerUTxOWordToCoinsPerUTxOByte :: CoinPerWord -> CoinPerByte
coinsPerUTxOWordToCoinsPerUTxOByte (CoinPerWord (Coin c)) = CoinPerByte $ Coin $ c `div` 8

-- | A word is 8 bytes, so to convert from coinsPerUTxOByte to coinsPerUTxOWord.
coinsPerUTxOByteToCoinsPerUTxOWord :: CoinPerByte -> CoinPerWord
coinsPerUTxOByteToCoinsPerUTxOWord (CoinPerByte (Coin c)) = CoinPerWord $ Coin $ c * 8

-- ======================================

instance ToExpr (BabbagePParams StrictMaybe era)

instance ToExpr (BabbagePParams Identity era)
