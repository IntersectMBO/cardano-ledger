{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module contains just the type of protocol parameters.
module Shelley.Spec.Ledger.PParams
  ( PParams(..)
  , emptyPParams
  -- lenses
  , minfeeA
  , minfeeB
  , maxBBSize
  , maxTxSize
  , maxBHSize
  , keyDeposit
  , keyMinRefund
  , keyDecayRate
  , poolDeposit
  , poolMinRefund
  , poolDecayRate
  , eMax
  , nOpt
  , a0
  , rho
  , tau
  , activeSlotCoeff
  , d
  , extraEntropy
  , protocolVersion
  , ActiveSlotCoeff
  , mkActiveSlotCoeff
  , activeSlotVal
  , activeSlotLog
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen, enforceSize)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.BaseTypes (FixedPoint, Nonce (NeutralNonce), UnitInterval,
                     fpPrecision, interval0, intervalValue)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Slot (EpochNo (..))

import           Shelley.Spec.NonIntegral (ln')

import           Lens.Micro.TH (makeLenses)

-- | Protocol parameters
data PParams = PParams
  { -- |The linear factor for the minimum fee calculation
    _minfeeA         :: Integer
    -- |The constant factor for the minimum fee calculation
  , _minfeeB         :: Natural
    -- | Maximal block body size
  , _maxBBSize       :: Natural
    -- | Maximal transaction size
  , _maxTxSize       :: Natural
    -- | Maximal block header size
  , _maxBHSize       :: Natural
    -- |The amount of a key registration deposit
  , _keyDeposit      :: Coin
    -- |The minimum percent refund guarantee
  , _keyMinRefund    :: UnitInterval
    -- |The deposit decay rate
  , _keyDecayRate    :: Rational
    -- |The amount of a pool registration deposit
  , _poolDeposit     :: Coin
    -- | The minimum percent pool refund
  , _poolMinRefund   :: UnitInterval
    -- | Decay rate for pool deposits
  , _poolDecayRate   :: Rational
    -- | epoch bound on pool retirement
  , _eMax            :: EpochNo
    -- | Desired number of pools
  , _nOpt            :: Natural
    -- | Pool influence
  , _a0              :: Rational
    -- | Treasury expansion
  , _rho             :: UnitInterval
    -- | Monetary expansion
  , _tau             :: UnitInterval
    -- | Active slot coefficient
  , _activeSlotCoeff :: ActiveSlotCoeff
    -- | Decentralization parameter
  , _d               :: UnitInterval
    -- | Extra entropy
  , _extraEntropy    :: Nonce
    -- | Protocol version
  , _protocolVersion :: (Natural, Natural)
  } deriving (Show, Eq, Generic)

data ActiveSlotCoeff =
  ActiveSlotCoeff
  { unActiveSlotVal :: UnitInterval
  , unActiveSlotLog :: Integer  -- TODO mgudemann make this FixedPoint,
                                -- currently a problem because of
                                -- NoUnexpectedThunks instance for FixedPoint
  } deriving (Eq, Ord, Show, Generic)

instance NoUnexpectedThunks ActiveSlotCoeff

instance FromCBOR ActiveSlotCoeff
 where
   fromCBOR = do
     v <- fromCBOR
     pure $ mkActiveSlotCoeff v

instance ToCBOR ActiveSlotCoeff
 where
   toCBOR (ActiveSlotCoeff { unActiveSlotVal = slotVal
                           , unActiveSlotLog = _logVal}) =
     toCBOR slotVal

mkActiveSlotCoeff :: UnitInterval -> ActiveSlotCoeff
mkActiveSlotCoeff v =
  ActiveSlotCoeff { unActiveSlotVal = v
                  , unActiveSlotLog =
                    if (intervalValue v) == 1
                      -- If the active slot coefficient is equal to one,
                      -- then nearly every stake pool can produce a block every slot.
                      -- In this degenerate case, where ln (1-f) is not defined,
                      -- we set the unActiveSlotLog to zero.
                      then 0
                      else floor (fpPrecision * (
                        ln' $ (1 :: FixedPoint) - (fromRational $ intervalValue v))) }

activeSlotVal :: ActiveSlotCoeff -> UnitInterval
activeSlotVal = unActiveSlotVal

activeSlotLog :: ActiveSlotCoeff -> FixedPoint
activeSlotLog f = (fromIntegral $ unActiveSlotLog f) / fpPrecision

instance NoUnexpectedThunks PParams

instance ToCBOR PParams
 where
  toCBOR (PParams
    { _minfeeA         = minfeeA'
    , _minfeeB         = minfeeB'
    , _maxBBSize       = maxBBSize'
    , _maxTxSize       = maxTxSize'
    , _maxBHSize       = maxBHSize'
    , _keyDeposit      = keyDeposit'
    , _keyMinRefund    = keyMinRefund'
    , _keyDecayRate    = keyDecayRate'
    , _poolDeposit     = poolDeposit'
    , _poolMinRefund   = poolMinRefund'
    , _poolDecayRate   = poolDecayRate'
    , _eMax            = eMax'
    , _nOpt            = nOpt'
    , _a0              = a0'
    , _rho             = rho'
    , _tau             = tau'
    , _activeSlotCoeff = activeSlotCoeff'
    , _d               = d'
    , _extraEntropy    = extraEntropy'
    , _protocolVersion = protocolVersion'
    }) =
      encodeListLen 20
        <> toCBOR minfeeA'
        <> toCBOR minfeeB'
        <> toCBOR maxBBSize'
        <> toCBOR maxTxSize'
        <> toCBOR maxBHSize'
        <> toCBOR keyDeposit'
        <> toCBOR keyMinRefund'
        <> toCBOR keyDecayRate'
        <> toCBOR poolDeposit'
        <> toCBOR poolMinRefund'
        <> toCBOR poolDecayRate'
        <> toCBOR eMax'
        <> toCBOR nOpt'
        <> toCBOR a0'
        <> toCBOR rho'
        <> toCBOR tau'
        <> toCBOR activeSlotCoeff'
        <> toCBOR d'
        <> toCBOR extraEntropy'
        <> toCBOR protocolVersion'

instance FromCBOR PParams
 where
  fromCBOR = do
    enforceSize "PParams" 20
    PParams
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

makeLenses ''PParams

-- | Returns a basic "empty" `PParams` structure with all zero values.
emptyPParams :: PParams
emptyPParams =
    PParams {
       _minfeeA = 0
     , _minfeeB = 0
     , _maxBBSize = 0
     , _maxTxSize = 2048
     , _maxBHSize = 0
     , _keyDeposit = Coin 0
     , _keyMinRefund = interval0
     , _keyDecayRate = 0
     , _poolDeposit = Coin 0
     , _poolMinRefund = interval0
     , _poolDecayRate = 0
     , _eMax = EpochNo 0
     , _nOpt = 100
     , _a0 = 0
     , _rho = interval0
     , _tau = interval0
     , _activeSlotCoeff = mkActiveSlotCoeff interval0
     , _d = interval0
     , _extraEntropy = NeutralNonce
     , _protocolVersion = (0, 0)
     }
