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

import Cardano.Ledger.Alonzo.Era
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    CostModels (..),
    ExUnits (..),
    Prices (..),
    getCostModelLanguage,
    getCostModelParams,
  )
import Cardano.Ledger.BaseTypes
  ( NonNegativeInterval,
    Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    fromSMaybe,
    isSNothing,
    natVersion,
  )
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
import Cardano.Ledger.Core hiding (PParams, PParamsUpdate)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.HKD (HKD)
import Cardano.Ledger.Orphans ()
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (ShelleyPParams))
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
    _minfeeA :: !(HKD f Natural),
    -- | The constant factor for the minimum fee calculation
    _minfeeB :: !(HKD f Natural),
    -- | Maximal block body size
    _maxBBSize :: !(HKD f Natural),
    -- | Maximal transaction size
    _maxTxSize :: !(HKD f Natural),
    -- | Maximal block header size
    _maxBHSize :: !(HKD f Natural),
    -- | The amount of a key registration deposit
    _keyDeposit :: !(HKD f Coin),
    -- | The amount of a pool registration deposit
    _poolDeposit :: !(HKD f Coin),
    -- | Maximum number of epochs in the future a pool retirement is allowed to
    -- be scheduled for.
    _eMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    _nOpt :: !(HKD f Natural),
    -- | Pool influence
    _a0 :: !(HKD f NonNegativeInterval),
    -- | Monetary expansion
    _rho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    _tau :: !(HKD f UnitInterval),
    -- | Decentralization parameter. Note that the scale is inverted here - a
    -- value of 0 indicates full decentralisation, where 1 indicates full
    -- federalisation.
    _d :: !(HKD f UnitInterval),
    -- | Extra entropy
    _extraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    _protocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum Stake Pool Cost
    _minPoolCost :: !(HKD f Coin),
    -- new/updated for alonzo

    -- | Cost in lovelace per word (8 bytes) of UTxO storage (instead of _minUTxOValue)
    _coinsPerUTxOWord :: !(HKD f Coin),
    -- | Cost models for non-native script languages
    _costmdls :: !(HKD f CostModels),
    -- | Prices of execution units (for non-native script languages)
    _prices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    _maxTxExUnits :: !(HKD f ExUnits),
    -- | Max total script execution resources units allowed per block
    _maxBlockExUnits :: !(HKD f ExUnits),
    -- | Max size of a Value in an output
    _maxValSize :: !(HKD f Natural),
    -- | Percentage of the txfee which must be provided as collateral when
    -- including non-native scripts.
    _collateralPercentage :: !(HKD f Natural),
    -- | Maximum number of collateral inputs allowed in a transaction
    _maxCollateralInputs :: !(HKD f Natural)
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
      { _minfeeA = minfeeA',
        _minfeeB = minfeeB',
        _maxBBSize = maxBBSize',
        _maxTxSize = maxTxSize',
        _maxBHSize = maxBHSize',
        _keyDeposit = keyDeposit',
        _poolDeposit = poolDeposit',
        _eMax = eMax',
        _nOpt = nOpt',
        _a0 = a0',
        _rho = rho',
        _tau = tau',
        _d = d',
        _extraEntropy = extraEntropy',
        _protocolVersion = protocolVersion',
        _minPoolCost = minPoolCost',
        -- new/updated for alonzo
        _coinsPerUTxOWord = coinsPerUTxOWord',
        _costmdls = costmdls',
        _prices = prices',
        _maxTxExUnits = maxTxExUnits',
        _maxBlockExUnits = maxBlockExUnits',
        _maxValSize = maxValSize',
        _collateralPercentage = collateralPercentage',
        _maxCollateralInputs = maxCollateralInputs'
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
        <! From -- _minfeeA         :: Integer
        <! From -- _minfeeB         :: Natural
        <! From -- _maxBBSize       :: Natural
        <! From -- _maxTxSize       :: Natural
        <! From -- _maxBHSize       :: Natural
        <! From -- _keyDeposit      :: Coin
        <! From -- _poolDeposit     :: Coin
        <! From -- _eMax            :: EpochNo
        <! From -- _nOpt            :: Natural
        <! From -- _a0              :: NonNegativeInterval
        <! From -- _rho             :: UnitInterval
        <! From -- _tau             :: UnitInterval
        <! From -- _d               :: UnitInterval
        <! From -- _extraEntropy    :: Nonce
        <! D fromCBORGroup -- _protocolVersion :: ProtVer
        <! From -- _minPoolCost     :: Natural
        -- new/updated for alonzo
        <! From -- _coinsPerUTxOWord  :: Coin
        <! From -- _costmdls :: CostModels
        <! From -- _prices = prices',
        <! From -- _maxTxExUnits = maxTxExUnits',
        <! From -- _maxBlockExUnits = maxBlockExUnits'
        <! From -- maxValSize :: Natural
        <! From -- collateralPercentage :: Natural
        <! From -- maxCollateralInputs :: Natural

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams era
emptyPParams =
  AlonzoPParams
    { _minfeeA = 0,
      _minfeeB = 0,
      _maxBBSize = 0,
      _maxTxSize = 2048,
      _maxBHSize = 0,
      _keyDeposit = Coin 0,
      _poolDeposit = Coin 0,
      _eMax = EpochNo 0,
      _nOpt = 100,
      _a0 = minBound,
      _rho = minBound,
      _tau = minBound,
      _d = minBound,
      _extraEntropy = NeutralNonce,
      _protocolVersion = BT.ProtVer (natVersion @5) 0,
      _minPoolCost = mempty,
      -- new/updated for alonzo
      _coinsPerUTxOWord = Coin 0,
      _costmdls = CostModels mempty,
      _prices = Prices minBound minBound,
      _maxTxExUnits = ExUnits 0 0,
      _maxBlockExUnits = ExUnits 0 0,
      _maxValSize = 0,
      _collateralPercentage = 150,
      _maxCollateralInputs = 5
    }

-- | Since ExUnits does not have an Ord instance, we have to roll this Ord instance by hand.
-- IF THE ORDER OR TYPES OF THE FIELDS OF PParams changes, this instance may need adusting.
instance Ord (AlonzoPParamsHKD StrictMaybe era) where
  compare x y =
    compare (_minfeeA x) (_minfeeA y)
      <> compare (_minfeeB x) (_minfeeB y)
      <> compare (_maxBBSize x) (_maxBBSize y)
      <> compare (_maxTxSize x) (_maxTxSize y)
      <> compare (_maxBHSize x) (_maxBHSize y)
      <> compare (_keyDeposit x) (_keyDeposit y)
      <> compare (_poolDeposit x) (_poolDeposit y)
      <> compare (_eMax x) (_eMax y)
      <> compare (_nOpt x) (_nOpt y)
      <> compare (_a0 x) (_a0 y)
      <> compare (_rho x) (_rho y)
      <> compare (_tau x) (_tau y)
      <> compare (_d x) (_d y)
      <> compare (_extraEntropy x) (_extraEntropy y)
      <> compare (_protocolVersion x) (_protocolVersion y)
      <> compare (_minPoolCost x) (_minPoolCost y)
      <> compare (_coinsPerUTxOWord x) (_coinsPerUTxOWord y)
      <> compare (_costmdls x) (_costmdls y)
      <> compare (_prices x) (_prices y)
      <> compareEx (_maxTxExUnits x) (_maxTxExUnits y)
      <> compareEx (_maxBlockExUnits x) (_maxBlockExUnits y)
      <> compare (_maxValSize x) (_maxValSize y)

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
    !> omitStrictMaybe 0 (_minfeeA ppup) toCBOR
    !> omitStrictMaybe 1 (_minfeeB ppup) toCBOR
    !> omitStrictMaybe 2 (_maxBBSize ppup) toCBOR
    !> omitStrictMaybe 3 (_maxTxSize ppup) toCBOR
    !> omitStrictMaybe 4 (_maxBHSize ppup) toCBOR
    !> omitStrictMaybe 5 (_keyDeposit ppup) toCBOR
    !> omitStrictMaybe 6 (_poolDeposit ppup) toCBOR
    !> omitStrictMaybe 7 (_eMax ppup) toCBOR
    !> omitStrictMaybe 8 (_nOpt ppup) toCBOR
    !> omitStrictMaybe 9 (_a0 ppup) toCBOR
    !> omitStrictMaybe 10 (_rho ppup) toCBOR
    !> omitStrictMaybe 11 (_tau ppup) toCBOR
    !> omitStrictMaybe 12 (_d ppup) toCBOR
    !> omitStrictMaybe 13 (_extraEntropy ppup) toCBOR
    !> omitStrictMaybe 14 (_protocolVersion ppup) toCBOR
    !> omitStrictMaybe 16 (_minPoolCost ppup) toCBOR
    !> omitStrictMaybe 17 (_coinsPerUTxOWord ppup) toCBOR
    !> omitStrictMaybe 18 (_costmdls ppup) toCBOR
    !> omitStrictMaybe 19 (_prices ppup) toCBOR
    !> omitStrictMaybe 20 (_maxTxExUnits ppup) toCBOR
    !> omitStrictMaybe 21 (_maxBlockExUnits ppup) toCBOR
    !> omitStrictMaybe 22 (_maxValSize ppup) toCBOR
    !> omitStrictMaybe 23 (_collateralPercentage ppup) toCBOR
    !> omitStrictMaybe 24 (_maxCollateralInputs ppup) toCBOR
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
    { _minfeeA = SNothing,
      _minfeeB = SNothing,
      _maxBBSize = SNothing,
      _maxTxSize = SNothing,
      _maxBHSize = SNothing,
      _keyDeposit = SNothing,
      _poolDeposit = SNothing,
      _eMax = SNothing,
      _nOpt = SNothing,
      _a0 = SNothing,
      _rho = SNothing,
      _tau = SNothing,
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minPoolCost = SNothing,
      -- new/updated for alonzo
      _coinsPerUTxOWord = SNothing,
      _costmdls = SNothing,
      _prices = SNothing,
      _maxTxExUnits = SNothing,
      _maxBlockExUnits = SNothing,
      _maxValSize = SNothing,
      _collateralPercentage = SNothing,
      _maxCollateralInputs = SNothing
    }

updateField :: Word -> Field (PParamsUpdate era)
updateField 0 = field (\x up -> up {_minfeeA = SJust x}) From
updateField 1 = field (\x up -> up {_minfeeB = SJust x}) From
updateField 2 = field (\x up -> up {_maxBBSize = SJust x}) From
updateField 3 = field (\x up -> up {_maxTxSize = SJust x}) From
updateField 4 = field (\x up -> up {_maxBHSize = SJust x}) From
updateField 5 = field (\x up -> up {_keyDeposit = SJust x}) From
updateField 6 = field (\x up -> up {_poolDeposit = SJust x}) From
updateField 7 = field (\x up -> up {_eMax = SJust x}) From
updateField 8 = field (\x up -> up {_nOpt = SJust x}) From
updateField 9 = field (\x up -> up {_a0 = SJust x}) From
updateField 10 = field (\x up -> up {_rho = SJust x}) From
updateField 11 = field (\x up -> up {_tau = SJust x}) From
updateField 12 = field (\x up -> up {_d = SJust x}) From
updateField 13 = field (\x up -> up {_extraEntropy = SJust x}) From
updateField 14 = field (\x up -> up {_protocolVersion = SJust x}) From
updateField 16 = field (\x up -> up {_minPoolCost = SJust x}) From
updateField 17 = field (\x up -> up {_coinsPerUTxOWord = SJust x}) From
updateField 18 = field (\x up -> up {_costmdls = SJust x}) From
updateField 19 = field (\x up -> up {_prices = SJust x}) From
updateField 20 = field (\x up -> up {_maxTxExUnits = SJust x}) From
updateField 21 = field (\x up -> up {_maxBlockExUnits = SJust x}) From
updateField 22 = field (\x up -> up {_maxValSize = SJust x}) From
updateField 23 = field (\x up -> up {_collateralPercentage = SJust x}) From
updateField 24 = field (\x up -> up {_maxCollateralInputs = SJust x}) From
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
    { _minfeeA = fromSMaybe (_minfeeA pp) (_minfeeA ppup),
      _minfeeB = fromSMaybe (_minfeeB pp) (_minfeeB ppup),
      _maxBBSize = fromSMaybe (_maxBBSize pp) (_maxBBSize ppup),
      _maxTxSize = fromSMaybe (_maxTxSize pp) (_maxTxSize ppup),
      _maxBHSize = fromSMaybe (_maxBHSize pp) (_maxBHSize ppup),
      _keyDeposit = fromSMaybe (_keyDeposit pp) (_keyDeposit ppup),
      _poolDeposit = fromSMaybe (_poolDeposit pp) (_poolDeposit ppup),
      _eMax = fromSMaybe (_eMax pp) (_eMax ppup),
      _nOpt = fromSMaybe (_nOpt pp) (_nOpt ppup),
      _a0 = fromSMaybe (_a0 pp) (_a0 ppup),
      _rho = fromSMaybe (_rho pp) (_rho ppup),
      _tau = fromSMaybe (_tau pp) (_tau ppup),
      _d = fromSMaybe (_d pp) (_d ppup),
      _extraEntropy = fromSMaybe (_extraEntropy pp) (_extraEntropy ppup),
      _protocolVersion = fromSMaybe (_protocolVersion pp) (_protocolVersion ppup),
      _minPoolCost = fromSMaybe (_minPoolCost pp) (_minPoolCost ppup),
      -- new/updated for alonzo
      _coinsPerUTxOWord = fromSMaybe (_coinsPerUTxOWord pp) (_coinsPerUTxOWord ppup),
      _costmdls = fromSMaybe (_costmdls pp) (_costmdls ppup),
      _prices = fromSMaybe (_prices pp) (_prices ppup),
      _maxTxExUnits = fromSMaybe (_maxTxExUnits pp) (_maxTxExUnits ppup),
      _maxBlockExUnits = fromSMaybe (_maxBlockExUnits pp) (_maxBlockExUnits ppup),
      _maxValSize = fromSMaybe (_maxValSize pp) (_maxValSize ppup),
      _collateralPercentage = fromSMaybe (_collateralPercentage pp) (_collateralPercentage ppup),
      _maxCollateralInputs = fromSMaybe (_maxCollateralInputs pp) (_maxCollateralInputs ppup)
    }

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
  forall era.
  ( HasField "_costmdls" (Core.PParams era) CostModels,
    HasField "_protocolVersion" (Core.PParams era) BT.ProtVer
  ) =>
  Core.PParams era ->
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
    costModel = Map.lookup lang (unCostModels $ getField @"_costmdls" pp)
    costModelEncoding = serializeEncoding' version $ maybe encodeNull encodeCostModel costModel
    version = BT.pvMajor $ getField @"_protocolVersion" pp

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
