{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains just the type of protocol parameters.
module Cardano.Ledger.Alonzo.PParams
  ( PParams' (..),
    PParams,
    emptyPParams,
    ProtVer (..),
    PParamsUpdate,
    emptyPParamsUpdate,
    updatePParams,
    getLanguageView,
    LangDepView (..),
  )
where

import Cardano.Binary
  ( Encoding,
    FromCBOR (..),
    ToCBOR (..),
    encodePreEncoded,
  )
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel,
    ExUnits (..),
    Prices (..),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Era
import Cardano.Ledger.Hashes (EraIndependentPParamView)
import Cardano.Ledger.SafeHash
  ( HashAnnotated (..),
    SafeToHash (..),
  )
import Control.DeepSeq (NFData)
import Data.ByteString.Short (fromShort)
import Data.Coders
  ( Decode (..),
    Density (..),
    Encode (..),
    Field (..),
    Wrapped (..),
    decode,
    encode,
    field,
    mapEncode,
    (!>),
    (<!),
  )
import Data.Default (Default (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.MemoBytes (MemoBytes (..), memoBytes)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (InspectHeapNamed (..), NoThunks (..))
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce (NeutralNonce),
    StrictMaybe (..),
    UnitInterval,
    interval0,
    strictMaybeToMaybe,
  )
import Shelley.Spec.Ledger.Orphans ()
import Shelley.Spec.Ledger.PParams (HKD, ProtVer (..))
import Shelley.Spec.Ledger.Serialization
  ( FromCBORGroup (..),
    ToCBORGroup (..),
    mapFromCBOR,
    mapToCBOR,
    ratioFromCBOR,
    ratioToCBOR,
  )
import Shelley.Spec.Ledger.Slot (EpochNo (..))

-- ================================================================
-- TODO make type families for PParams and PParamsUpdate

-- How to handle this alonzo-specific type?
type PParamsUpdate era = PParams' StrictMaybe era

-- | Protocol parameters.
-- Shelley parameters + additional ones
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
    -- | epoch bound on pool retirement
    _eMax :: !(HKD f EpochNo),
    -- | Desired number of pools
    _nOpt :: !(HKD f Natural),
    -- | Pool influence
    _a0 :: !(HKD f Rational),
    -- | Monetary expansion
    _rho :: !(HKD f UnitInterval),
    -- | Treasury expansion
    _tau :: !(HKD f UnitInterval),
    -- | Decentralization parameter
    _d :: !(HKD f UnitInterval),
    -- | Extra entropy
    _extraEntropy :: !(HKD f Nonce),
    -- | Protocol version
    _protocolVersion :: !(HKD f ProtVer),
    -- | Minimum Stake Pool Cost
    _minPoolCost :: !(HKD f Coin),
    -- new/updated for alonzo

    -- | Cost in ada per byte of UTxO storage (instead of _minUTxOValue)
    _adaPerUTxOWord :: !(HKD f Coin),
    -- | Cost models for non-native script languages
    _costmdls :: !(HKD f (Map Language CostModel)),
    -- | Prices of execution units (for non-native script languages)
    _prices :: !(HKD f Prices),
    -- | Max total script execution resources units allowed per tx
    _maxTxExUnits :: !(HKD f ExUnits),
    -- | Max total script execution resources units allowed per block
    _maxBlockExUnits :: !(HKD f ExUnits),
    -- | Max size of a Value in an output
    _maxValSize :: !(HKD f Natural)
  }
  deriving (Generic)

type PParams era = PParams' Identity era

deriving instance Eq (PParams' Identity era)

deriving instance Show (PParams' Identity era)

deriving instance NFData (PParams' Identity era)

instance NoThunks (PParams era)

instance (Era era) => ToCBOR (PParams era) where
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
        _d = d',
        _extraEntropy = extraEntropy',
        _protocolVersion = protocolVersion',
        _minPoolCost = minPoolCost',
        -- new/updated for alonzo
        _adaPerUTxOWord = adaPerUTxOWord',
        _costmdls = costmdls',
        _prices = prices',
        _maxTxExUnits = maxTxExUnits',
        _maxBlockExUnits = maxBlockExUnits',
        _maxValSize = maxValSize'
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
            !> E ratioToCBOR a0'
            !> To rho'
            !> To tau'
            !> To d'
            !> To extraEntropy'
            !> E toCBORGroup protocolVersion'
            !> To minPoolCost'
            -- new/updated for alonzo
            !> To adaPerUTxOWord'
            !> mapEncode costmdls'
            !> To prices'
            !> To maxTxExUnits'
            !> To maxBlockExUnits'
            !> To maxValSize'
        )

instance
  (Era era) =>
  FromCBOR (PParams era)
  where
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
        <! (D ratioFromCBOR) -- _a0 :: Rational
        <! From -- _rho             :: UnitInterval
        <! From -- _tau             :: UnitInterval
        <! From -- _d               :: UnitInterval
        <! From -- _extraEntropy    :: Nonce
        <! (D fromCBORGroup) -- _protocolVersion :: ProtVer
        <! From -- _minPoolCost     :: Natural
        -- new/updated for alonzo
        <! From -- _adaPerUTxOWord  :: Coin
        <! (D mapFromCBOR) -- _costmdls :: (Map Language CostModel)
        <! From -- _prices = prices',
        <! From -- _maxTxExUnits = maxTxExUnits',
        <! From -- _maxBlockExUnits = maxBlockExUnits'
        <! From -- maxValSize :: Natural

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
      _a0 = 0,
      _rho = interval0,
      _tau = interval0,
      _d = interval0,
      _extraEntropy = NeutralNonce,
      _protocolVersion = ProtVer 0 0,
      _minPoolCost = mempty,
      -- new/updated for alonzo
      _adaPerUTxOWord = Coin 0,
      _costmdls = mempty,
      _prices = Prices (Coin 0) (Coin 0),
      _maxTxExUnits = ExUnits 0 0,
      _maxBlockExUnits = ExUnits 0 0,
      _maxValSize = 0
    }

-- | Since ExUnits does not have an Ord instance, we have to roll this Ord instance by hand.
-- IF THE ORDER OR TYPES OF THE FIELDS OF PParams changes, this instance may need adusting.
instance Ord (PParams' StrictMaybe era) where
  compare x y =
    (_minfeeA x, _minfeeA y)
      <== (_minfeeB x, _minfeeB y)
      <== (_maxBBSize x, _maxBBSize y)
      <== (_maxTxSize x, _maxTxSize y)
      <== (_maxBHSize x, _maxBHSize y)
      <== (_keyDeposit x, _keyDeposit y)
      <== (_poolDeposit x, _poolDeposit y)
      <== (_eMax x, _eMax y)
      <== (_nOpt x, _nOpt y)
      <== (_a0 x, _a0 y)
      <== (_rho x, _rho y)
      <== (_tau x, _tau y)
      <== (_d x, _d y)
      <== (_extraEntropy x, _extraEntropy y)
      <== (_protocolVersion x, _protocolVersion y)
      <== (_minPoolCost x, _minPoolCost y)
      <== (_adaPerUTxOWord x, _adaPerUTxOWord y)
      <== (_costmdls x, _costmdls y)
      <== (_prices x, _prices y)
      <== ( case compareEx (_maxTxExUnits x) (_maxTxExUnits y) of
              LT -> LT
              GT -> GT
              EQ -> case compareEx (_maxBlockExUnits x) (_maxBlockExUnits y) of
                LT -> LT
                GT -> GT
                EQ -> (_maxValSize x, _maxValSize y) <== EQ
          )

infixr 4 <==

(<==) :: Ord a => (a, a) -> Ordering -> Ordering
(x, y) <== z = case compare x y of LT -> LT; GT -> GT; EQ -> z

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
    !> omitStrictMaybe 9 (_a0 ppup) ratioToCBOR
    !> omitStrictMaybe 10 (_rho ppup) toCBOR
    !> omitStrictMaybe 11 (_tau ppup) toCBOR
    !> omitStrictMaybe 12 (_d ppup) toCBOR
    !> omitStrictMaybe 13 (_extraEntropy ppup) toCBOR
    !> omitStrictMaybe 14 (_protocolVersion ppup) toCBOR
    !> omitStrictMaybe 16 (_minPoolCost ppup) toCBOR
    !> omitStrictMaybe 17 (_adaPerUTxOWord ppup) toCBOR
    !> omitStrictMaybe 18 (_costmdls ppup) mapToCBOR
    !> omitStrictMaybe 19 (_prices ppup) toCBOR
    !> omitStrictMaybe 20 (_maxTxExUnits ppup) toCBOR
    !> omitStrictMaybe 21 (_maxBlockExUnits ppup) toCBOR
    !> omitStrictMaybe 22 (_maxValSize ppup) toCBOR
  where
    omitStrictMaybe ::
      Word -> StrictMaybe a -> (a -> Encoding) -> Encode ('Closed 'Sparse) (StrictMaybe a)
    omitStrictMaybe key x enc = Omit isSNothing (Key key (E (enc . fromSJust) x))

    fromSJust :: StrictMaybe a -> a
    fromSJust (SJust x) = x
    fromSJust SNothing = error "SNothing in fromSJust. This should never happen, it is guarded by isSNothing."

    isSNothing :: StrictMaybe a -> Bool
    isSNothing SNothing = True
    isSNothing (SJust _) = False

instance (Era era) => ToCBOR (PParamsUpdate era) where
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
      _d = SNothing,
      _extraEntropy = SNothing,
      _protocolVersion = SNothing,
      _minPoolCost = SNothing,
      -- new/updated for alonzo
      _adaPerUTxOWord = SNothing,
      _costmdls = SNothing,
      _prices = SNothing,
      _maxTxExUnits = SNothing,
      _maxBlockExUnits = SNothing,
      _maxValSize = SNothing
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
updateField 9 = field (\x up -> up {_a0 = x}) (D $ SJust <$> ratioFromCBOR)
updateField 10 = field (\x up -> up {_rho = SJust x}) From
updateField 11 = field (\x up -> up {_tau = SJust x}) From
updateField 12 = field (\x up -> up {_d = SJust x}) From
updateField 13 = field (\x up -> up {_extraEntropy = SJust x}) From
updateField 14 = field (\x up -> up {_protocolVersion = SJust x}) From
updateField 16 = field (\x up -> up {_minPoolCost = SJust x}) From
updateField 17 = field (\x up -> up {_adaPerUTxOWord = SJust x}) From
updateField 18 = field (\x up -> up {_costmdls = x}) (D $ SJust <$> mapFromCBOR)
updateField 19 = field (\x up -> up {_prices = SJust x}) From
updateField 20 = field (\x up -> up {_maxTxExUnits = SJust x}) From
updateField 21 = field (\x up -> up {_maxBlockExUnits = SJust x}) From
updateField 22 = field (\x up -> up {_maxValSize = SJust x}) From
updateField k = field (\_x up -> up) (Invalid k)

instance (Era era) => FromCBOR (PParamsUpdate era) where
  fromCBOR =
    decode
      (SparseKeyed "PParamsUpdate" emptyPParamsUpdate updateField [])

-- =================================================================

-- | Update operation for protocol parameters structure @PParams
updatePParams :: PParams era -> PParamsUpdate era -> PParams era
updatePParams pp ppup =
  PParams
    { _minfeeA = fromMaybe' (_minfeeA pp) (_minfeeA ppup),
      _minfeeB = fromMaybe' (_minfeeB pp) (_minfeeB ppup),
      _maxBBSize = fromMaybe' (_maxBBSize pp) (_maxBBSize ppup),
      _maxTxSize = fromMaybe' (_maxTxSize pp) (_maxTxSize ppup),
      _maxBHSize = fromMaybe' (_maxBHSize pp) (_maxBHSize ppup),
      _keyDeposit = fromMaybe' (_keyDeposit pp) (_keyDeposit ppup),
      _poolDeposit = fromMaybe' (_poolDeposit pp) (_poolDeposit ppup),
      _eMax = fromMaybe' (_eMax pp) (_eMax ppup),
      _nOpt = fromMaybe' (_nOpt pp) (_nOpt ppup),
      _a0 = fromMaybe' (_a0 pp) (_a0 ppup),
      _rho = fromMaybe' (_rho pp) (_rho ppup),
      _tau = fromMaybe' (_tau pp) (_tau ppup),
      _d = fromMaybe' (_d pp) (_d ppup),
      _extraEntropy = fromMaybe' (_extraEntropy pp) (_extraEntropy ppup),
      _protocolVersion = fromMaybe' (_protocolVersion pp) (_protocolVersion ppup),
      _minPoolCost = fromMaybe' (_minPoolCost pp) (_minPoolCost ppup),
      -- new/updated for alonzo
      _adaPerUTxOWord = fromMaybe' (_adaPerUTxOWord pp) (_adaPerUTxOWord ppup),
      _costmdls = fromMaybe' (_costmdls pp) (_costmdls ppup),
      _prices = fromMaybe' (_prices pp) (_prices ppup),
      _maxTxExUnits = fromMaybe' (_maxTxExUnits pp) (_maxTxExUnits ppup),
      _maxBlockExUnits = fromMaybe' (_maxBlockExUnits pp) (_maxBlockExUnits ppup),
      _maxValSize = fromMaybe' (_maxValSize pp) (_maxValSize ppup)
    }
  where
    fromMaybe' :: a -> StrictMaybe a -> a
    fromMaybe' x = fromMaybe x . strictMaybeToMaybe

-- ===================================================
-- Figure 1: "Definitions Used in Protocol Parameters"

-- The Language Depenedent View type (LangDepView) is a GADT, it will have a
-- different Constructor for each Language. This way each language can have a
-- different view of the Protocol parameters. This is an intermediate type and
-- we introduce it only because we are interested in combining it with other
-- things so we can hash the combination. Thus this type (and the other things
-- we combine it with) must remember their original bytes. We make it a data
-- type around a MemoBytes. Unfortunately we can't use a newtype, because the
-- constructor must existentially hide the Language index. Instances on GADT's
-- are a bit tricky so we are carefull in the comments below to explain them.
-- Because we intend to add new languages the GADT will someday end up with more
-- constructors. So we add some commented out code that suggests how to extend
-- the instances when that happens.

data RawView :: Type -> Language -> Type where
  RawPlutusView :: CostModel -> RawView era 'PlutusV1

-- RawOtherView :: Int -> RawView era 'OtherLANG

deriving instance Eq (RawView era lang)

deriving instance Ord (RawView era lang)

deriving instance Show (RawView era lang)

deriving instance Typeable (RawView era lang)

deriving via InspectHeapNamed "RawView" (RawView e l) instance NoThunks (RawView e l)

instance (Typeable era) => ToCBOR (LangDepView era) where
  toCBOR (LangDepViewConstr (Memo _ bytes)) = encodePreEncoded (fromShort bytes)

pattern PlutusView :: CostModel -> LangDepView era
pattern PlutusView cm <-
  LangDepViewConstr (Memo (RawPlutusView cm) _)
  where
    PlutusView cm = LangDepViewConstr (memoBytes (Sum RawPlutusView 0 !> To cm))

-- Note the tag 0 ^
{- How to extend to another language
pattern OtherView :: Int -> LangDepView era
pattern OtherView n <- LangDepViewConstr (Memo (RawOtherView cm) _)
  where OtherView n = LangDepViewConstr (memoBytes (Sum RawOtherView 1 !> To n))
                                                  -- Note the tag 1  ^
-}

{-# COMPLETE PlutusView {- OtherView -} #-}

instance (Typeable era) => FromCBOR (LangDepView era) where
  fromCBOR = decode $ Summands "LangDepView" decodeTag
    where
      decodeTag :: Word -> Decode 'Open (LangDepView era)
      decodeTag 0 = SumD PlutusView <! From
      -- decodeTag 1 = SumD OtherView <! From
      -- Since Int has FromCBOR instance
      decodeTag n = Invalid n

data LangDepView era where
  LangDepViewConstr :: (MemoBytes (RawView era lang)) -> LangDepView era

-- We can't derive SafeToHash via newtype deriving because LandDepViewConstr is
-- a data not a newtype. But it does remember its original bytes so we can add
-- the instance manually.

instance SafeToHash (LangDepView era) where
  originalBytes (LangDepViewConstr (Memo _ bs)) = fromShort bs

instance
  (Crypto era ~ c) =>
  HashAnnotated (LangDepView era) EraIndependentPParamView c

deriving via
  InspectHeapNamed "LangDepView" (LangDepView e)
  instance
    NoThunks (LangDepView e)

instance Show (LangDepView era) where
  show (PlutusView x) = show x

-- show (OtherView n) = Show n

instance Eq (LangDepView era) where
  (PlutusView x) == (PlutusView y) = x == y

-- (OtherView n) == (OtherView m) = n==m
--  _ == _ = False

instance Ord (LangDepView era) where
  compare (PlutusView x) (PlutusView y) = compare x y

-- compare (OtherView n) (OtherView m) = compare n m
-- compare x y = compare (rank x) (rank y)
--   where rank :: LangDepView era -> Int
--         rank (PlutusView _) = 1
--         rank (OtherView _) = 2

getLanguageView ::
  forall era.
  PParams era ->
  Language ->
  Maybe (LangDepView era)
getLanguageView pp PlutusV1 = PlutusView <$> Map.lookup PlutusV1 (_costmdls pp)
