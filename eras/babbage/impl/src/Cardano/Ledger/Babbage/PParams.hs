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

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Babbage.PParams
  ( PParams' (..),
    PParams,
    emptyPParams,
    PParamsUpdate,
    emptyPParamsUpdate,
    updatePParams,
    getLanguageView,
    LangDepView (..),
    encodeLangViews,
    retractPP,
    extendPP,
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
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
  )
import Cardano.Ledger.Shelley.Orphans ()
import Cardano.Ledger.Shelley.PParams (HKD)
import qualified Cardano.Ledger.Shelley.PParams as Shelley (PParams' (..))
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

type PParamsUpdate era = PParams' StrictMaybe era

-- | Protocol parameters.
-- Alonzo parameters without d and extraEntropy
data PParams' f era = PParams
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
    -- | Protocol version
    _protocolVersion :: !(HKD f BT.ProtVer),
    -- | Minimum Stake Pool Cost
    _minPoolCost :: !(HKD f Coin),
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

type PParams = PParams' Identity

deriving instance Eq (PParams' Identity era)

deriving instance Show (PParams' Identity era)

deriving instance NFData (PParams' Identity era)

instance NoThunks (PParams era)

instance Era era => ToCBOR (PParams era) where
  toCBOR
    PParams
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
        _protocolVersion = protocolVersion',
        _minPoolCost = minPoolCost',
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
        ( Rec (PParams @Identity)
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
            !> To coinsPerUTxOWord'
            !> To costmdls'
            !> To prices'
            !> To maxTxExUnits'
            !> To maxBlockExUnits'
            !> To maxValSize'
            !> To collateralPercentage'
            !> To maxCollateralInputs'
        )

instance Era era => FromCBOR (PParams era) where
  fromCBOR =
    decode $
      RecD PParams
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
        <! D fromCBORGroup -- _protocolVersion :: ProtVer
        <! From -- _minPoolCost     :: Natural
        <! From -- _coinsPerUTxOWord  :: Coin
        <! From -- _costmdls :: CostModel
        <! From -- _prices = prices',
        <! From -- _maxTxExUnits = maxTxExUnits',
        <! From -- _maxBlockExUnits = maxBlockExUnits'
        <! From -- maxValSize :: Natural
        <! From -- collateralPercentage :: Natural
        <! From -- maxCollateralInputs :: Natural

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams era
emptyPParams =
  PParams
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
      _protocolVersion = BT.ProtVer 0 0,
      _minPoolCost = mempty,
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
instance Ord (PParams' StrictMaybe era) where
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

deriving instance Eq (PParams' StrictMaybe era)

deriving instance Show (PParams' StrictMaybe era)

deriving instance NFData (PParams' StrictMaybe era)

instance NoThunks (PParamsUpdate era)

-- =======================================================
-- A PParamsUpdate has StrictMaybe fields, we want to Sparse encode it, by
-- writing only those fields where the field is (SJust x), that is the role of
-- the local function (omitStrictMaybe key x)

encodePParamsUpdate ::
  PParamsUpdate era ->
  Encode ('Closed 'Sparse) (PParamsUpdate era)
encodePParamsUpdate ppup =
  Keyed PParams
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

instance Era era => ToCBOR (PParamsUpdate era) where
  toCBOR ppup = encode (encodePParamsUpdate ppup)

emptyPParamsUpdate :: PParamsUpdate era
emptyPParamsUpdate =
  PParams
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
      _protocolVersion = SNothing,
      _minPoolCost = SNothing,
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

instance Era era => FromCBOR (PParamsUpdate era) where
  fromCBOR =
    decode
      (SparseKeyed "PParamsUpdate" emptyPParamsUpdate updateField [])

-- =================================================================

-- | Update operation for protocol parameters structure @PParams
updatePParams :: PParams era -> PParamsUpdate era -> PParams era
updatePParams pp ppup =
  PParams
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
      _protocolVersion = fromSMaybe (_protocolVersion pp) (_protocolVersion ppup),
      _minPoolCost = fromSMaybe (_minPoolCost pp) (_minPoolCost ppup),
      _coinsPerUTxOWord = fromSMaybe (_coinsPerUTxOWord pp) (_coinsPerUTxOWord ppup),
      _costmdls = fromSMaybe (_costmdls pp) (_costmdls ppup),
      _prices = fromSMaybe (_prices pp) (_prices ppup),
      _maxTxExUnits = fromSMaybe (_maxTxExUnits pp) (_maxTxExUnits ppup),
      _maxBlockExUnits = fromSMaybe (_maxBlockExUnits pp) (_maxBlockExUnits ppup),
      _maxValSize = fromSMaybe (_maxValSize pp) (_maxValSize ppup),
      _collateralPercentage = fromSMaybe (_collateralPercentage pp) (_collateralPercentage ppup),
      _maxCollateralInputs = fromSMaybe (_maxCollateralInputs pp) (_maxCollateralInputs ppup)
    }

-- | Turn an PParams' into a Shelley.Params'
retractPP :: HKD f Coin -> HKD f UnitInterval -> HKD f Nonce -> PParams' f era -> Shelley.PParams' f era
retractPP
  c
  d
  eE
  (PParams ma mb mxBB mxT mxBH kd pd emx a n rho tau pv mnP _ _ _ _ _ _ _ _) =
    Shelley.PParams ma mb mxBB mxT mxBH kd pd emx a n rho tau d eE pv c mnP

-- | Given the missing pieces Turn a Shelley.PParams' into an Params'
extendPP ::
  Shelley.PParams' f era1 ->
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
  (Shelley.PParams ma mb mxBB mxT mxBH kd pd emx a n rho tau _d _eE pv _ mnP)
  ada
  cost
  price
  mxTx
  mxBl
  mxV
  col
  mxCol =
    PParams ma mb mxBB mxT mxBH kd pd emx a n rho tau pv mnP ada cost price mxTx mxBl mxV col mxCol

-- | Since Babbage removes the '_d' field from PParams, we provide this
-- 'HasField' instance which defaults '_d' to '0' in order to reuse
-- code for the reward calculation.
instance HasField "_d" (PParams era) UnitInterval where
  getField _ = minBound
