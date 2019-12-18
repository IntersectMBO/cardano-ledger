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

import           BaseTypes
import           Cardano.Binary (FromCBOR (..), ToCBOR, toCBOR)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Trans.Reader (asks)
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Keys (KeyHash, Sig, VKey, VKeyES)
import           Numeric.Natural (Natural)
import           Slot (SlotNo (..))

import           Serialization (CBORGroup (..), FromCBORGroup (..), ToCBORGroup (..))

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
  => FromCBORGroup (OCert crypto)
 where
  fromCBORGroup =
    OCert
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

kesPeriod :: SlotNo -> ShelleyBase KESPeriod
kesPeriod (SlotNo s) = asks slotsPerKESPeriod <&> \spkp ->
  KESPeriod . fromIntegral $ s `div` spkp
