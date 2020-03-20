{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Shelley.Spec.Ledger.Updates
  ( Ppm(..)
  , PPUpdateEnv(..)
  , PPUpdate(..)
  , emptyPPUpdate
  , PParamsUpdate(..)
  , Update(..)
  , updatePParams
  )
where

import           Cardano.Binary (Encoding, FromCBOR (..), ToCBOR (..), encodeListLen, encodeMapLen,
                     enforceSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Shelley.Spec.Ledger.BaseTypes (Nonce, UnitInterval)
import           Shelley.Spec.Ledger.Coin (Coin)
import           Shelley.Spec.Ledger.Keys (GenDelegs, GenKeyHash)
import           Shelley.Spec.Ledger.PParams (ActiveSlotCoeff, PParams (..), ProtVer)
import           Shelley.Spec.Ledger.Serialization (decodeMapContents, mapFromCBOR, mapToCBOR,
                     rationalFromCBOR, rationalToCBOR)
import           Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo)

import           Numeric.Natural (Natural)


-- | Update Proposal
data Update crypto
  = Update (PPUpdate crypto) EpochNo
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (Update crypto)

instance Crypto crypto => ToCBOR (Update crypto) where
  toCBOR (Update ppUpdate e) =
    encodeListLen 2 <> toCBOR ppUpdate <> toCBOR e

instance Crypto crypto => FromCBOR (Update crypto) where
  fromCBOR =
    Update <$ enforceSize "Update" 2
      <*> fromCBOR
      <*> fromCBOR

data PPUpdateEnv crypto = PPUpdateEnv {
    slotNo :: SlotNo
  , genDelegs  :: GenDelegs crypto
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PPUpdateEnv crypto)

-- | Protocol parameter selector, contains just a single field to be updated
--   within @PPUpdate
data Ppm = MinFeeA Integer
  | MinFeeB Natural
  | MaxBBSize Natural
  | MaxTxSize Natural
  | MaxBHSize Natural
  | KeyDeposit Coin
  | KeyMinRefund UnitInterval
  | KeyDecayRate Rational
  | PoolDeposit Coin
  | PoolMinRefund UnitInterval
  | PoolDecayRate Rational
  | EMax EpochNo
  | Nopt Natural
  | A0 Rational
  | Rho UnitInterval
  | Tau UnitInterval
  | ActiveSlotCoefficient ActiveSlotCoeff
  | D UnitInterval
  | ExtraEntropy Nonce
  | ProtocolVersion ProtVer
  deriving (Show, Ord, Eq, Generic)

instance NoUnexpectedThunks Ppm

newtype PParamsUpdate = PParamsUpdate { ppmSet :: Set Ppm }
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks PParamsUpdate

instance ToCBOR PParamsUpdate where
  toCBOR (PParamsUpdate ppms) = encodeMapLen l <> foldMap f ppms
    where
      l = fromIntegral $ Set.size ppms
      word :: Word8 -> Encoding
      word = toCBOR
      f = \case
        MinFeeA x               -> word  0 <> toCBOR x
        MinFeeB x               -> word  1 <> toCBOR x
        MaxBBSize x             -> word  2 <> toCBOR x
        MaxTxSize x             -> word  3 <> toCBOR x
        MaxBHSize x             -> word  4 <> toCBOR x
        KeyDeposit x            -> word  5 <> toCBOR x
        KeyMinRefund x          -> word  6 <> toCBOR x
        KeyDecayRate x          -> word  7 <> rationalToCBOR x
        PoolDeposit x           -> word  8 <> toCBOR x
        PoolMinRefund x         -> word  9 <> toCBOR x
        PoolDecayRate x         -> word 10 <> rationalToCBOR x
        EMax x                  -> word 11 <> toCBOR x
        Nopt x                  -> word 12 <> toCBOR x
        A0   x                  -> word 13 <> rationalToCBOR x
        Rho  x                  -> word 14 <> toCBOR x
        Tau  x                  -> word 15 <> toCBOR x
        ActiveSlotCoefficient x -> word 16 <> toCBOR x
        D x                     -> word 17 <> toCBOR x
        ExtraEntropy x          -> word 18 <> toCBOR x
        ProtocolVersion x       -> word 19 <> toCBOR x

instance FromCBOR PParamsUpdate where
  fromCBOR = fmap (PParamsUpdate . Set.fromList)
    $ decodeMapContents
    $ fromCBOR @Word8 >>= \case
         0  -> MinFeeA <$> fromCBOR
         1  -> MinFeeB <$> fromCBOR
         2  -> MaxBBSize <$> fromCBOR
         3  -> MaxTxSize <$> fromCBOR
         4  -> MaxBHSize <$> fromCBOR
         5  -> KeyDeposit <$> fromCBOR
         6  -> KeyMinRefund <$> fromCBOR
         7  -> KeyDecayRate <$> rationalFromCBOR
         8  -> PoolDeposit <$> fromCBOR
         9  -> PoolMinRefund <$> fromCBOR
         10 -> PoolDecayRate <$> rationalFromCBOR
         11 -> (EMax . EpochNo) <$> fromCBOR
         12 -> Nopt <$> fromCBOR
         13 -> A0   <$> rationalFromCBOR
         14 -> Rho  <$> fromCBOR
         15 -> Tau  <$> fromCBOR
         16 -> ActiveSlotCoefficient <$> fromCBOR
         17 -> D <$> fromCBOR
         18 -> ExtraEntropy <$> fromCBOR
         19 -> ProtocolVersion <$> fromCBOR
         k -> fail $ "not a valid key: " ++ show k

-- | Update operation for protocol parameters structure @PParams
newtype PPUpdate crypto
  = PPUpdate (Map (GenKeyHash crypto) PParamsUpdate)
  deriving (Show, Eq, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (PPUpdate crypto) where
  toCBOR (PPUpdate m) = mapToCBOR m

instance Crypto crypto => FromCBOR (PPUpdate crypto) where
  fromCBOR = PPUpdate <$> mapFromCBOR

emptyPPUpdate :: PPUpdate crypto
emptyPPUpdate = PPUpdate Map.empty

updatePParams :: PParams -> PParamsUpdate -> PParams
updatePParams ppms (PParamsUpdate up) = Set.foldr updatePParams' ppms up
 where
  updatePParams' (MinFeeA               p) pps = pps { _minfeeA = p }
  updatePParams' (MinFeeB               p) pps = pps { _minfeeB = p }
  updatePParams' (MaxBBSize             p) pps = pps { _maxBBSize = p }
  updatePParams' (MaxTxSize             p) pps = pps { _maxTxSize = p }
  updatePParams' (MaxBHSize             p) pps = pps { _maxBHSize = p }
  updatePParams' (KeyDeposit            p) pps = pps { _keyDeposit = p }
  updatePParams' (KeyMinRefund          p) pps = pps { _keyMinRefund = p }
  updatePParams' (KeyDecayRate          p) pps = pps { _keyDecayRate = p }
  updatePParams' (PoolDeposit           p) pps = pps { _poolDeposit = p }
  updatePParams' (PoolMinRefund         p) pps = pps { _poolMinRefund = p }
  updatePParams' (PoolDecayRate         p) pps = pps { _poolDecayRate = p }
  updatePParams' (EMax                  p) pps = pps { _eMax = p }
  updatePParams' (Nopt                  p) pps = pps { _nOpt = p }
  updatePParams' (A0                    p) pps = pps { _a0 = p }
  updatePParams' (Rho                   p) pps = pps { _rho = p }
  updatePParams' (Tau                   p) pps = pps { _tau = p }
  updatePParams' (ActiveSlotCoefficient p) pps = pps { _activeSlotCoeff = p }
  updatePParams' (D                     p) pps = pps { _d = p }
  updatePParams' (ExtraEntropy          p) pps = pps { _extraEntropy = p }
  updatePParams' (ProtocolVersion       p) pps = pps { _protocolVersion = p }
