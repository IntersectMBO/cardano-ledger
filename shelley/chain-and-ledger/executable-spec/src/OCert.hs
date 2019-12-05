{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module OCert
  ( OCert(..)
  , OCertEnv(..)
  , currentIssueNo
  , KESPeriod(..)
  , slotsPerKESPeriod
  , kesPeriod)
where

import           Cardano.Binary (FromCBOR(..), ToCBOR
                                , enforceSize, toCBOR)
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Cardano.Ledger.Shelley.Crypto
import           Keys (KeyHash, Sig, VKey, VKeyES)
import           Slot (Slot (..))

import           Numeric.Natural (Natural)

import           Serialization (ToCBORGroup (..), CBORGroup (..))

data OCertEnv crypto = OCertEnv
  { ocertEnvStPools :: Set (KeyHash crypto)
  , ocertEnvGenDelegs :: Set (KeyHash crypto)
  } deriving (Show, Eq)

currentIssueNo
  :: OCertEnv crypto
  -> (Map (KeyHash crypto) Natural)
  -> KeyHash crypto -- ^ Pool hash
  -> Maybe Natural
currentIssueNo (OCertEnv stPools genDelegs) cs hk
  | Map.member hk cs = Map.lookup hk cs
  | Set.member hk stPools = Just 0
  | Set.member hk genDelegs = Just 0
  | otherwise = Nothing

newtype KESPeriod = KESPeriod Natural
  deriving (Show, Eq, Ord, NoUnexpectedThunks, FromCBOR, ToCBOR)

data OCert crypto = OCert
  { -- | The operational hot key
    ocertVkHot     :: VKeyES crypto
    -- | The cold key
  , ocertVkCold    :: VKey crypto
    -- | counter
  , ocertN         :: Natural
    -- | Start of key evolving signature period
  , ocertKESPeriod :: KESPeriod
    -- | Signature of block operational certificate content
  , ocertSigma     :: Sig crypto (VKeyES crypto, Natural, KESPeriod)
  } deriving (Show, Eq, Generic)
    deriving ToCBOR via (CBORGroup (OCert crypto))

instance Crypto crypto => NoUnexpectedThunks (OCert crypto)

instance
  (Crypto crypto)
  => ToCBORGroup (OCert crypto)
 where
  toCBORGroup ocert =
         toCBOR (ocertVkHot ocert)
      <> toCBOR (ocertVkCold ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> toCBOR (ocertSigma ocert)
  listLen _ = 5

instance
  (Crypto crypto)
  => FromCBOR (OCert crypto)
 where
  fromCBOR = enforceSize "OCert should have 5 fields" 5 >>
    OCert
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

slotsPerKESPeriod :: Natural
slotsPerKESPeriod = 90

kesPeriod :: Slot -> KESPeriod
kesPeriod (Slot s) = KESPeriod $ s `div` slotsPerKESPeriod
