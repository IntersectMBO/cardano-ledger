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

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Alonzo.PParams
  ( AlonzoPParamsHKD (..),
    AlonzoPParams,
    emptyPParams,
    AlonzoPParamsUpdate,
    emptyPParamsUpdate,
    updatePParams,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    retractPP,
    extendPP,

    -- * Deprecated
    PParams',
    PParams,
    PParamsUpdate,
  )
where

import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
    encodeMapLen,
    encodeNull,
    encodePreEncoded,
    serialize',
    serializeEncoding',
  )
import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    CostModels (..),
    ExUnits (..),
    Prices (..),
    getCostModelParams,
  )
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    fromSMaybe,
    isSNothing,
  )
import qualified Cardano.Ledger.BaseTypes as BT (ProtVer (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (PParams, PParamsUpdate)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Serialization (FromCBORGroup (..), ToCBORGroup (..))
import Cardano.Ledger.Shelley.Orphans ()
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (ShelleyPParams))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field (..),
    Wrapped (..),
    decode,
    encode,
    encodeFoldableAsIndefinite,
    field,
    (!>),
    (<!),
  )
import Data.Default (Default (..))
import Data.Function (on)
import Data.Functor.Identity (Identity (..))
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

type PParams era = AlonzoPParams era

{-# DEPRECATED PParams "Use `AlonzoPParams` instead" #-}

type PParams' f era = AlonzoPParamsHKD f era

{-# DEPRECATED PParams' "Use `AlonzoPParamsHKD` instead" #-}

type PParamsUpdate era = AlonzoPParamsUpdate era

{-# DEPRECATED PParamsUpdate "Use `AlonzoPParamsUpdate` instead" #-}

-- | Protocol parameters.
-- Shelley parameters + additional ones
data AlonzoPParamsHKD f era = AlonzoPParams
  { -- | The linear factor for the minimum fee calculation
    appMinfeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    appMinfeeB :: !(HKD f Natural),
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

    -- | Cost in lovelace per word (8 bytes) of UTxO storage (instead of _minUTxOValue)
    appCoinsPerUTxOWord :: !(HKD f Coin),
    -- | Cost models for non-native script languages
    appCostmdls :: !(HKD f CostModels),
    -- | Prices of execution units (for non-native script languages)
    appPrices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    appMaxTxExUnits :: !(HKD f ExUnits),
    -- | Max total script execution resources units allowed per block
    appMaxBlockExUnits :: !(HKD f ExUnits),
    -- | Max size of a Value in an output
    appMaxValSize :: !(HKD f Natural),
    -- | Percentage of the txfee which must be provided as collateral when
    -- including non-native scripts.
    appCollateralPercentage :: !(HKD f Natural),
    -- | Maximum number of collateral inputs allowed in a transaction
    appMaxCollateralInputs :: !(HKD f Natural)
  }
  deriving (Generic)

type AlonzoPParams era = AlonzoPParamsHKD Identity era

type AlonzoPParamsUpdate era = AlonzoPParamsHKD StrictMaybe era

instance CC.Crypto c => EraPParams (AlonzoEra c) where
  type PParams (AlonzoEra c) = AlonzoPParams (AlonzoEra c)
  type PParamsUpdate (AlonzoEra c) = AlonzoPParamsUpdate (AlonzoEra c)

  applyPPUpdates = updatePParams

deriving instance Eq (PParams' Identity era)

deriving instance Show (PParams' Identity era)

deriving instance NFData (PParams' Identity era)

instance NoThunks (PParams era)

instance (Era era) => ToCBOR (PParams era) where
  toCBOR
    AlonzoPParams
      { appMinfeeA = minfeeA',
        appMinfeeB = minfeeB',
        appMaxBBSize = maxBBSize',
        appMaxTxSize = maxTxSize',
        appMaxBHSize = maxBHSize',
        appKeyDeposit = keyDeposit',
        appPoolDeposit = poolDeposit',
        appEMax = eMax',
        appNOpt = nOpt',
        appA0 = a0',
        appRho = rho',
        appTau = tau',
        appD = d',
        appExtraEntropy = extraEntropy',
        appProtocolVersion = protocolVersion',
        appMinPoolCost = minPoolCost',
        -- new/updated for alonzo
        appCoinsPerUTxOWord = coinsPerUTxOWord',
        appCostmdls = costmdls',
        appPrices = prices',
        appMaxTxExUnits = maxTxExUnits',
        appMaxBlockExUnits = maxBlockExUnits',
        appMaxValSize = maxValSize',
        appCollateralPercentage = collateralPercentage',
        appMaxCollateralInputs = maxCollateralInputs'
      } =
      encode
        ( Rec (AlonzoPParams @Identity)
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
            !> To d'
            !> To extraEntropy'
            !> E toCBORGroup protocolVersion'
            !> To minPoolCost'
            -- new/updated for alonzo
            !> To coinsPerUTxOWord'
            !> To costmdls'
            !> To prices'
            !> To maxTxExUnits'
            !> To maxBlockExUnits'
            !> To maxValSize'
            !> To collateralPercentage'
            !> To maxCollateralInputs'
        )

instance
  (Era era) =>
  FromCBOR (PParams era)
  where
  fromCBOR =
    decode $
      RecD AlonzoPParams
        <! From -- appMinfeeA         :: Integer
        <! From -- appMinfeeB         :: Natural
        <! From -- appMaxBBSize       :: Natural
        <! From -- appMaxTxSize       :: Natural
        <! From -- appMaxBHSize       :: Natural
        <! From -- appKeyDeposit      :: Coin
        <! From -- appPoolDeposit     :: Coin
        <! From -- appEMax            :: EpochNo
        <! From -- appNOpt            :: Natural
        <! From -- appA0              :: NonNegativeInterval
        <! From -- appRho             :: UnitInterval
        <! From -- appTau             :: UnitInterval
        <! From -- appD               :: UnitInterval
        <! From -- appExtraEntropy    :: Nonce
        <! D fromCBORGroup -- sppProtocolVersion :: ProtVer
        <! From -- appMinPoolCost     :: Natural
        -- new/updated for alonzo
        <! From -- appCoinsPerUTxOWord  :: Coin
        <! From -- appCostmdls :: CostModels
        <! From -- appPrices = prices',
        <! From -- appMaxTxExUnits = maxTxExUnits',
        <! From -- appMaxBlockExUnits = maxBlockExUnits'
        <! From -- maxValSize :: Natural
        <! From -- collateralPercentage :: Natural
        <! From -- maxCollateralInputs :: Natural

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams era
emptyPParams =
  AlonzoPParams
    { appMinfeeA = 0,
      appMinfeeB = 0,
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
      appProtocolVersion = BT.ProtVer 5 0,
      appMinPoolCost = mempty,
      -- new/updated for alonzo
      appCoinsPerUTxOWord = Coin 0,
      appCostmdls = CostModels mempty,
      appPrices = Prices minBound minBound,
      appMaxTxExUnits = ExUnits 0 0,
      appMaxBlockExUnits = ExUnits 0 0,
      appMaxValSize = 0,
      appCollateralPercentage = 150,
      appMaxCollateralInputs = 5
    }

-- | Since ExUnits does not have an Ord instance, we have to roll this Ord instance by hand.
-- IF THE ORDER OR TYPES OF THE FIELDS OF PParams changes, this instance may need adusting.
instance Ord (AlonzoPParamsHKD StrictMaybe era) where
  compare x y =
    compare (appMinfeeA x) (appMinfeeA y)
      <> compare (appMinfeeB x) (appMinfeeB y)
      <> compare (appMaxBBSize x) (appMaxBBSize y)
      <> compare (appMaxTxSize x) (appMaxTxSize y)
      <> compare (appMaxBHSize x) (appMaxBHSize y)
      <> compare (appKeyDeposit x) (appKeyDeposit y)
      <> compare (appPoolDeposit x) (appPoolDeposit y)
      <> compare (appEMax x) (appEMax y)
      <> compare (appNOpt x) (appNOpt y)
      <> compare (appA0 x) (appA0 y)
      <> compare (appRho x) (appRho y)
      <> compare (appTau x) (appTau y)
      <> compare (appD x) (appD y)
      <> compare (appExtraEntropy x) (appExtraEntropy y)
      <> compare (appProtocolVersion x) (appProtocolVersion y)
      <> compare (appMinPoolCost x) (appMinPoolCost y)
      <> compare (appCoinsPerUTxOWord x) (appCoinsPerUTxOWord y)
      <> compare (appCostmdls x) (appCostmdls y)
      <> compare (appPrices x) (appPrices y)
      <> compareEx (appMaxTxExUnits x) (appMaxTxExUnits y)
      <> compareEx (appMaxBlockExUnits x) (appMaxBlockExUnits y)
      <> compare (appMaxValSize x) (appMaxValSize y)

compareEx :: StrictMaybe ExUnits -> StrictMaybe ExUnits -> Ordering
compareEx SNothing SNothing = EQ
compareEx SNothing (SJust _) = LT
compareEx (SJust _) SNothing = GT
compareEx (SJust (ExUnits m1 s1)) (SJust (ExUnits m2 s2)) = compare (m1, s1) (m2, s2)

instance Default (PParams era) where
  def = emptyPParams

deriving instance Eq (AlonzoPParamsHKD StrictMaybe era)

deriving instance Show (AlonzoPParamsHKD StrictMaybe era)

deriving instance NFData (AlonzoPParamsHKD StrictMaybe era)

instance NoThunks (AlonzoPParamsUpdate era)

-- =======================================================
-- A PParamsUpdate has StrictMaybe fields, we want to Sparse encode it, by
-- writing only those fields where the field is (SJust x), that is the role of
-- the local function (omitStrictMaybe key x)

encodePParamsUpdate ::
  AlonzoPParamsUpdate era ->
  Encode ('Closed 'Sparse) (AlonzoPParamsUpdate era)
encodePParamsUpdate ppup =
  Keyed AlonzoPParams
    !> omitStrictMaybe 0 (appMinfeeA ppup) toCBOR
    !> omitStrictMaybe 1 (appMinfeeB ppup) toCBOR
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
    !> omitStrictMaybe 18 (appCostmdls ppup) toCBOR
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

instance (Era era) => ToCBOR (PParamsUpdate era) where
  toCBOR ppup = encode (encodePParamsUpdate ppup)

emptyPParamsUpdate :: PParamsUpdate era
emptyPParamsUpdate =
  AlonzoPParams
    { appMinfeeA = SNothing,
      appMinfeeB = SNothing,
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
      appCostmdls = SNothing,
      appPrices = SNothing,
      appMaxTxExUnits = SNothing,
      appMaxBlockExUnits = SNothing,
      appMaxValSize = SNothing,
      appCollateralPercentage = SNothing,
      appMaxCollateralInputs = SNothing
    }

updateField :: Word -> Field (PParamsUpdate era)
updateField 0 = field (\x up -> up {appMinfeeA = SJust x}) From
updateField 1 = field (\x up -> up {appMinfeeB = SJust x}) From
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
updateField 18 = field (\x up -> up {appCostmdls = SJust x}) From
updateField 19 = field (\x up -> up {appPrices = SJust x}) From
updateField 20 = field (\x up -> up {appMaxTxExUnits = SJust x}) From
updateField 21 = field (\x up -> up {appMaxBlockExUnits = SJust x}) From
updateField 22 = field (\x up -> up {appMaxValSize = SJust x}) From
updateField 23 = field (\x up -> up {appCollateralPercentage = SJust x}) From
updateField 24 = field (\x up -> up {appMaxCollateralInputs = SJust x}) From
updateField k = field (\_x up -> up) (Invalid k)

instance (Era era) => FromCBOR (PParamsUpdate era) where
  fromCBOR =
    decode
      (SparseKeyed "PParamsUpdate" emptyPParamsUpdate updateField [])

-- =================================================================

-- | Update operation for protocol parameters structure @PParams
updatePParams :: PParams era -> PParamsUpdate era -> PParams era
updatePParams pp ppup =
  AlonzoPParams
    { appMinfeeA = fromSMaybe (appMinfeeA pp) (appMinfeeA ppup),
      appMinfeeB = fromSMaybe (appMinfeeB pp) (appMinfeeB ppup),
      appMaxBBSize = fromSMaybe (appMaxBBSize pp) (appMaxBBSize ppup),
      appMaxTxSize = fromSMaybe (appMaxTxSize pp) (appMaxTxSize ppup),
      appMaxBHSize = fromSMaybe (appMaxBHSize pp) (appMaxBHSize ppup),
      appKeyDeposit = fromSMaybe (appKeyDeposit pp) (appKeyDeposit ppup),
      appPoolDeposit = fromSMaybe (appPoolDeposit pp) (appPoolDeposit ppup),
      appEMax = fromSMaybe (appEMax pp) (appEMax ppup),
      appNOpt = fromSMaybe (appNOpt pp) (appNOpt ppup),
      appA0 = fromSMaybe (appA0 pp) (appA0 ppup),
      appRho = fromSMaybe (appRho pp) (appRho ppup),
      appTau = fromSMaybe (appTau pp) (appTau ppup),
      appD = fromSMaybe (appD pp) (appD ppup),
      appExtraEntropy = fromSMaybe (appExtraEntropy pp) (appExtraEntropy ppup),
      appProtocolVersion = fromSMaybe (appProtocolVersion pp) (appProtocolVersion ppup),
      appMinPoolCost = fromSMaybe (appMinPoolCost pp) (appMinPoolCost ppup),
      -- new/updated for alonzo
      appCoinsPerUTxOWord = fromSMaybe (appCoinsPerUTxOWord pp) (appCoinsPerUTxOWord ppup),
      appCostmdls = fromSMaybe (appCostmdls pp) (appCostmdls ppup),
      appPrices = fromSMaybe (appPrices pp) (appPrices ppup),
      appMaxTxExUnits = fromSMaybe (appMaxTxExUnits pp) (appMaxTxExUnits ppup),
      appMaxBlockExUnits = fromSMaybe (appMaxBlockExUnits pp) (appMaxBlockExUnits ppup),
      appMaxValSize = fromSMaybe (appMaxValSize pp) (appMaxValSize ppup),
      appCollateralPercentage = fromSMaybe (appCollateralPercentage pp) (appCollateralPercentage ppup),
      appMaxCollateralInputs = fromSMaybe (appMaxCollateralInputs pp) (appMaxCollateralInputs ppup)
    }

-- ===================================================
-- Figure 1: "Definitions Used in Protocol Parameters"

-- The LangDepView is a key value pair. The key is the (canonically) encoded
-- language tag and the value is the (canonically) encoded set of relevant
-- protocol parameters
data LangDepView = LangDepView {tag :: ByteString, params :: ByteString}
  deriving (Eq, Show, Ord, Generic, NoThunks)

-- In the Alonzo era, the map of languages to cost models was mistakenly encoded
-- using an indefinite CBOR map (contrary to canonical CBOR, as intended) when
-- computing the script integrity hash.
-- For this reason, PlutusV1 remains with this encoding.
-- Future versions of Plutus, starting with PlutusV2 in the Babbage era, will
-- use the intended definite length encoding.
legacyNonCanonicalCostModelEncoder :: CostModel -> Encoding
legacyNonCanonicalCostModelEncoder = encodeFoldableAsIndefinite . getCostModelParams

getLanguageView ::
  forall era.
  (HasField "appCostmdls" (Core.PParams era) CostModels) =>
  Core.PParams era ->
  Language ->
  LangDepView
getLanguageView pp lang@PlutusV1 =
  LangDepView -- The silly double bagging is to keep compatibility with a past bug
    (serialize' (serialize' lang))
    ( serialize'
        ( serializeEncoding' $
            maybe encodeNull legacyNonCanonicalCostModelEncoder $
              Map.lookup lang (unCostModels $ getField @"appCostmdls" pp)
        )
    )
getLanguageView pp lang@PlutusV2 =
  LangDepView
    (serialize' lang)
    ( serializeEncoding' $
        maybe encodeNull toCBOR $
          Map.lookup lang (unCostModels $ getField @"appCostmdls" pp)
    )

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

-- | Turn an PParams' into a ShelleyParams'
retractPP :: HKD f Coin -> AlonzoPParamsHKD f era2 -> ShelleyPParamsHKD f era1
retractPP
  c
  (AlonzoPParams ma mb mxBB mxT mxBH kd pd emx a n rho tau d eE pv mnP _ _ _ _ _ _ _ _) =
    ShelleyPParams ma mb mxBB mxT mxBH kd pd emx a n rho tau d eE pv c mnP

-- | Given the missing pieces Turn a ShelleyPParams' into an Params'
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
  PParams' f era2
extendPP
  (ShelleyPParams ma mb mxBB mxT mxBH kd pd emx a n rho tau d eE pv _ mnP)
  ada
  cost
  price
  mxTx
  mxBl
  mxV
  col
  mxCol =
    AlonzoPParams ma mb mxBB mxT mxBH kd pd emx a n rho tau d eE pv mnP ada cost price mxTx mxBl mxV col mxCol
