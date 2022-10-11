{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains the type of protocol parameters and EraPParams instance
module Cardano.Ledger.Babbage.PParams
  ( BabbagePParamsHKD (..),
    BabbagePParams,
    emptyPParams,
    BabbagePParamsUpdate,
    emptyPParamsUpdate,
    updatePParams,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    retractPP,
    extendPP,

    -- * Dperecated
    PParams,
    PParams',
    PParamsUpdate,
  )
where

import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
  )
import Cardano.Ledger.Alonzo.PParams (LangDepView (..), encodeLangViews, getLanguageView)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModels (..),
    ExUnits (..),
    Prices (..),
  )
import Cardano.Ledger.Babbage.Era (BabbageEra)
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    Nonce,
    StrictMaybe (..),
    UnitInterval,
    fromSMaybe,
    isSNothing,
  )
import qualified Cardano.Ledger.BaseTypes as BT (ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era, EraPParams (applyPPUpdates))
import qualified Cardano.Ledger.Core as Core (PParams, PParamsUpdate)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
  )
import Cardano.Ledger.Shelley.Orphans ()
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (ShelleyPParams))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.Coders
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
import Data.Default (Default (..))
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

type PParams era = BabbagePParams era

{-# DEPRECATED PParams "Use `BabbagePParams` instead" #-}

type PParams' f era = BabbagePParamsHKD f era

{-# DEPRECATED PParams' "Use `BabbagePParamsHKD` instead" #-}

type PParamsUpdate era = BabbagePParamsUpdate era

{-# DEPRECATED PParamsUpdate "Use `BabbagePParamsUpdate` instead" #-}

-- | Protocol parameters.
-- Alonzo parameters without d and extraEntropy
data BabbagePParamsHKD f era = BabbagePParams
  { -- | The linear factor for the minimum fee calculation
    bppMinfeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    bppMinfeeB :: !(HKD f Natural),
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
    bppProtocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum Stake Pool Cost
    bppMinPoolCost :: !(HKD f Coin),
    -- | Cost in lovelace per byte of UTxO storage (instead of bppCoinsPerUTxOByte)
    bppCoinsPerUTxOByte :: !(HKD f Coin),
    -- | Cost models for non-native script languages
    bppCostmdls :: !(HKD f CostModels),
    -- | Prices of execution units (for non-native script languages)
    bppPrices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    bppMaxTxExUnits :: !(HKD f ExUnits),
    -- | Max total script execution resources units allowed per block
    bppMaxBlockExUnits :: !(HKD f ExUnits),
    -- | Max size of a Value in an output
    bppMaxValSize :: !(HKD f Natural),
    -- | Percentage of the txfee which must be provided as collateral when
    -- including non-native scripts.
    bppCollateralPercentage :: !(HKD f Natural),
    -- | Maximum number of collateral inputs allowed in a transaction
    bppMaxCollateralInputs :: !(HKD f Natural)
  }
  deriving (Generic)

type BabbagePParams = BabbagePParamsHKD Identity

type BabbagePParamsUpdate era = BabbagePParamsHKD StrictMaybe era

instance CC.Crypto c => EraPParams (BabbageEra c) where
  type PParams (BabbageEra c) = BabbagePParams (BabbageEra c)
  type PParamsUpdate (BabbageEra c) = BabbagePParamsUpdate (BabbageEra c)
  applyPPUpdates = updatePParams

deriving instance Eq (BabbagePParams era)

deriving instance Show (BabbagePParams era)

deriving instance NFData (BabbagePParams era)

instance NoThunks (BabbagePParams era)

instance Era era => ToCBOR (BabbagePParams era) where
  toCBOR
    BabbagePParams
      { bppMinfeeA = minfeeA',
        bppMinfeeB = minfeeB',
        bppMaxBBSize = maxBBSize',
        bppMaxTxSize = maxTxSize',
        bppMaxBHSize = maxBHSize',
        bppKeyDeposit = keyDeposit',
        bppPoolDeposit = poolDeposit',
        bppEMax = eMax',
        bppNOpt = nOpt',
        bppA0 = a0',
        bppRho = rho',
        bppTau = tau',
        bppProtocolVersion = protocolVersion',
        bppMinPoolCost = minPoolCost',
        bppCoinsPerUTxOByte = coinsPerUTxOByte',
        bppCostmdls = costmdls',
        bppPrices = prices',
        bppMaxTxExUnits = maxTxExUnits',
        bppMaxBlockExUnits = maxBlockExUnits',
        bppMaxValSize = maxValSize',
        bppCollateralPercentage = collateralPercentage',
        bppMaxCollateralInputs = maxCollateralInputs'
      } =
      encode
        ( Rec (BabbagePParams @Identity)
            !> To minfeeA'
            !> To minfeeB'
            !> To maxBBSize'
            !> To maxTxSize'
            !> To maxBHSize'
            !> To keyDeposit'
            !> To poolDeposit'
            !> To eMax'
            !> To nOpt'
            !> To a0'
            !> To rho'
            !> To tau'
            !> E toCBORGroup protocolVersion'
            !> To minPoolCost'
            !> To coinsPerUTxOByte'
            !> To costmdls'
            !> To prices'
            !> To maxTxExUnits'
            !> To maxBlockExUnits'
            !> To maxValSize'
            !> To collateralPercentage'
            !> To maxCollateralInputs'
        )

instance Era era => FromCBOR (BabbagePParams era) where
  fromCBOR =
    decode $
      RecD BabbagePParams
        <! From -- bppMinfeeA         :: Integer
        <! From -- bppMinfeeB         :: Natural
        <! From -- bppMaxBBSize       :: Natural
        <! From -- bppMaxTxSize       :: Natural
        <! From -- bppMaxBHSize       :: Natural
        <! From -- bppKeyDeposit      :: Coin
        <! From -- bppPoolDeposit     :: Coin
        <! From -- bppEMax            :: EpochNo
        <! From -- bppNOpt            :: Natural
        <! From -- bppA0              :: NonNegativeInterval
        <! From -- bppRho             :: UnitInterval
        <! From -- bppTau             :: UnitInterval
        <! D fromCBORGroup -- bppProtocolVersion :: ProtVer
        <! From -- bppMinPoolCost     :: Natural
        <! From -- bppCoinsPerUTxOByte  :: Coin
        <! From -- bppCostmdls :: CostModel
        <! From -- bppPrices = prices',
        <! From -- bppMaxTxExUnits = maxTxExUnits',
        <! From -- bppMaxBlockExUnits = maxBlockExUnits'
        <! From -- maxValSize :: Natural
        <! From -- collateralPercentage :: Natural
        <! From -- maxCollateralInputs :: Natural

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: BabbagePParams era
emptyPParams =
  BabbagePParams
    { bppMinfeeA = 0,
      bppMinfeeB = 0,
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
      bppProtocolVersion = BT.ProtVer 7 0,
      bppMinPoolCost = mempty,
      bppCoinsPerUTxOByte = Coin 0,
      bppCostmdls = CostModels mempty,
      bppPrices = Prices minBound minBound,
      bppMaxTxExUnits = ExUnits 0 0,
      bppMaxBlockExUnits = ExUnits 0 0,
      bppMaxValSize = 0,
      bppCollateralPercentage = 150,
      bppMaxCollateralInputs = 5
    }

-- | Since ExUnits does not have an Ord instance, we have to roll this Ord instance by hand.
-- IF THE ORDER OR TYPES OF THE FIELDS OF PParams changes, this instance may need adusting.
instance Ord (BabbagePParamsHKD StrictMaybe era) where
  compare x y =
    compare (bppMinfeeA x) (bppMinfeeA y)
      <> compare (bppMinfeeB x) (bppMinfeeB y)
      <> compare (bppMaxBBSize x) (bppMaxBBSize y)
      <> compare (bppMaxTxSize x) (bppMaxTxSize y)
      <> compare (bppMaxBHSize x) (bppMaxBHSize y)
      <> compare (bppKeyDeposit x) (bppKeyDeposit y)
      <> compare (bppPoolDeposit x) (bppPoolDeposit y)
      <> compare (bppEMax x) (bppEMax y)
      <> compare (bppNOpt x) (bppNOpt y)
      <> compare (bppA0 x) (bppA0 y)
      <> compare (bppRho x) (bppRho y)
      <> compare (bppTau x) (bppTau y)
      <> compare (bppProtocolVersion x) (bppProtocolVersion y)
      <> compare (bppMinPoolCost x) (bppMinPoolCost y)
      <> compare (bppCoinsPerUTxOByte x) (bppCoinsPerUTxOByte y)
      <> compare (bppCostmdls x) (bppCostmdls y)
      <> compare (bppPrices x) (bppPrices y)
      <> compareEx (bppMaxTxExUnits x) (bppMaxTxExUnits y)
      <> compareEx (bppMaxBlockExUnits x) (bppMaxBlockExUnits y)
      <> compare (bppMaxValSize x) (bppMaxValSize y)

compareEx :: StrictMaybe ExUnits -> StrictMaybe ExUnits -> Ordering
compareEx SNothing SNothing = EQ
compareEx SNothing (SJust _) = LT
compareEx (SJust _) SNothing = GT
compareEx (SJust (ExUnits m1 s1)) (SJust (ExUnits m2 s2)) = compare (m1, s1) (m2, s2)

instance Default (BabbagePParams era) where
  def = emptyPParams

deriving instance Eq (BabbagePParamsUpdate era)

deriving instance Show (BabbagePParamsUpdate era)

deriving instance NFData (BabbagePParamsUpdate era)

instance NoThunks (BabbagePParamsUpdate era)

-- =======================================================
-- A PParamsUpdate has StrictMaybe fields, we want to Sparse encode it, by
-- writing only those fields where the field is (SJust x), that is the role of
-- the local function (omitStrictMaybe key x)

encodePParamsUpdate ::
  BabbagePParamsUpdate era ->
  Encode ('Closed 'Sparse) (BabbagePParamsUpdate era)
encodePParamsUpdate ppup =
  Keyed BabbagePParams
    !> omitStrictMaybe 0 (bppMinfeeA ppup) toCBOR
    !> omitStrictMaybe 1 (bppMinfeeB ppup) toCBOR
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
    !> omitStrictMaybe 18 (bppCostmdls ppup) toCBOR
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

instance Era era => ToCBOR (BabbagePParamsUpdate era) where
  toCBOR ppup = encode (encodePParamsUpdate ppup)

emptyPParamsUpdate :: BabbagePParamsUpdate era
emptyPParamsUpdate =
  BabbagePParams
    { bppMinfeeA = SNothing,
      bppMinfeeB = SNothing,
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
      bppCostmdls = SNothing,
      bppPrices = SNothing,
      bppMaxTxExUnits = SNothing,
      bppMaxBlockExUnits = SNothing,
      bppMaxValSize = SNothing,
      bppCollateralPercentage = SNothing,
      bppMaxCollateralInputs = SNothing
    }

updateField :: Word -> Field (BabbagePParamsUpdate era)
updateField 0 = field (\x up -> up {bppMinfeeA = SJust x}) From
updateField 1 = field (\x up -> up {bppMinfeeB = SJust x}) From
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
updateField 18 = field (\x up -> up {bppCostmdls = SJust x}) From
updateField 19 = field (\x up -> up {bppPrices = SJust x}) From
updateField 20 = field (\x up -> up {bppMaxTxExUnits = SJust x}) From
updateField 21 = field (\x up -> up {bppMaxBlockExUnits = SJust x}) From
updateField 22 = field (\x up -> up {bppMaxValSize = SJust x}) From
updateField 23 = field (\x up -> up {bppCollateralPercentage = SJust x}) From
updateField 24 = field (\x up -> up {bppMaxCollateralInputs = SJust x}) From
updateField k = field (\_x up -> up) (Invalid k)

instance Era era => FromCBOR (BabbagePParamsUpdate era) where
  fromCBOR =
    decode
      (SparseKeyed "PParamsUpdate" emptyPParamsUpdate updateField [])

-- =================================================================

-- | Update operation for protocol parameters structure @PParams
updatePParams :: BabbagePParams era -> BabbagePParamsUpdate era -> BabbagePParams era
updatePParams pp ppup =
  BabbagePParams
    { bppMinfeeA = fromSMaybe (bppMinfeeA pp) (bppMinfeeA ppup),
      bppMinfeeB = fromSMaybe (bppMinfeeB pp) (bppMinfeeB ppup),
      bppMaxBBSize = fromSMaybe (bppMaxBBSize pp) (bppMaxBBSize ppup),
      bppMaxTxSize = fromSMaybe (bppMaxTxSize pp) (bppMaxTxSize ppup),
      bppMaxBHSize = fromSMaybe (bppMaxBHSize pp) (bppMaxBHSize ppup),
      bppKeyDeposit = fromSMaybe (bppKeyDeposit pp) (bppKeyDeposit ppup),
      bppPoolDeposit = fromSMaybe (bppPoolDeposit pp) (bppPoolDeposit ppup),
      bppEMax = fromSMaybe (bppEMax pp) (bppEMax ppup),
      bppNOpt = fromSMaybe (bppNOpt pp) (bppNOpt ppup),
      bppA0 = fromSMaybe (bppA0 pp) (bppA0 ppup),
      bppRho = fromSMaybe (bppRho pp) (bppRho ppup),
      bppTau = fromSMaybe (bppTau pp) (bppTau ppup),
      bppProtocolVersion = fromSMaybe (bppProtocolVersion pp) (bppProtocolVersion ppup),
      bppMinPoolCost = fromSMaybe (bppMinPoolCost pp) (bppMinPoolCost ppup),
      bppCoinsPerUTxOByte = fromSMaybe (bppCoinsPerUTxOByte pp) (bppCoinsPerUTxOByte ppup),
      bppCostmdls = fromSMaybe (bppCostmdls pp) (bppCostmdls ppup),
      bppPrices = fromSMaybe (bppPrices pp) (bppPrices ppup),
      bppMaxTxExUnits = fromSMaybe (bppMaxTxExUnits pp) (bppMaxTxExUnits ppup),
      bppMaxBlockExUnits = fromSMaybe (bppMaxBlockExUnits pp) (bppMaxBlockExUnits ppup),
      bppMaxValSize = fromSMaybe (bppMaxValSize pp) (bppMaxValSize ppup),
      bppCollateralPercentage = fromSMaybe (bppCollateralPercentage pp) (bppCollateralPercentage ppup),
      bppMaxCollateralInputs = fromSMaybe (bppMaxCollateralInputs pp) (bppMaxCollateralInputs ppup)
    }

-- | Turn an BabbagePParamsHKD into a ShelleyPParamsHKD
retractPP ::
  HKD f Coin ->
  HKD f UnitInterval ->
  HKD f Nonce ->
  BabbagePParamsHKD f era ->
  ShelleyPParamsHKD f era
retractPP
  c
  d
  eE
  (BabbagePParams ma mb mxBB mxT mxBH kd pd emx a n rho tau pv mnP _ _ _ _ _ _ _ _) =
    ShelleyPParams ma mb mxBB mxT mxBH kd pd emx a n rho tau d eE pv c mnP

-- | Given the missing pieces Turn a ShelleyPParamsHKD into an BabbagePParamsHKD
extendPP ::
  ShelleyPParamsHKD f era1 ->
  HKD f Coin ->
  HKD f CostModels ->
  HKD f Prices ->
  HKD f ExUnits ->
  HKD f ExUnits ->
  HKD f Natural ->
  HKD f Natural ->
  HKD f Natural ->
  BabbagePParamsHKD f era2
extendPP
  (ShelleyPParams ma mb mxBB mxT mxBH kd pd emx a n rho tau _d _eE pv _ mnP)
  ada
  cost
  price
  mxTx
  mxBl
  mxV
  col
  mxCol =
    BabbagePParams ma mb mxBB mxT mxBH kd pd emx a n rho tau pv mnP ada cost price mxTx mxBl mxV col mxCol

-- | Since Babbage removes the '_d' field from PParams, we provide this
-- 'HasField' instance which defaults '_d' to '0' in order to reuse
-- code for the reward calculation.
instance HasField "_d" (BabbagePParams era) UnitInterval where
  getField _ = minBound
