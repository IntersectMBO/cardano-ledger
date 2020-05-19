{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shelley.Spec.Ledger.OCert
  ( OCert (..),
    OCertEnv (..),
    currentIssueNo,
    KESPeriod (..),
    slotsPerKESPeriod,
    kesPeriod,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), toCBOR)
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.KES as KES
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
  ( KeyHash,
    KeyRole (..),
    SignedDSIGN,
    VerKeyKES,
    coerceKeyRole,
    decodeSignedDSIGN,
    decodeVerKeyKES,
    encodeSignedDSIGN,
    encodeVerKeyKES,
  )
import Shelley.Spec.Ledger.Serialization
  ( CBORGroup (..),
    FromCBORGroup (..),
    ToCBORGroup (..),
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))

data OCertEnv crypto = OCertEnv
  { ocertEnvStPools :: Set (KeyHash 'StakePool crypto),
    ocertEnvGenDelegs :: Set (KeyHash 'GenesisDelegate crypto)
  }
  deriving (Show, Eq)

currentIssueNo ::
  OCertEnv crypto ->
  (Map (KeyHash 'BlockIssuer crypto) Natural) ->
  -- | Pool hash
  KeyHash 'BlockIssuer crypto ->
  Maybe Natural
currentIssueNo (OCertEnv stPools genDelegs) cs hk
  | Map.member hk cs = Map.lookup hk cs
  | Set.member (coerceKeyRole hk) stPools = Just 0
  | Set.member (coerceKeyRole hk) genDelegs = Just 0
  | otherwise = Nothing

newtype KESPeriod = KESPeriod Word
  deriving (Show, Eq, Ord, NoUnexpectedThunks, FromCBOR, ToCBOR)

data OCert crypto = OCert
  { -- | The operational hot key
    ocertVkHot :: !(VerKeyKES crypto),
    -- | counter
    ocertN :: !Natural,
    -- | Start of key evolving signature period
    ocertKESPeriod :: !KESPeriod,
    -- | Signature of block operational certificate content
    ocertSigma :: !(SignedDSIGN crypto (VerKeyKES crypto, Natural, KESPeriod))
  }
  deriving (Generic)
  deriving (ToCBOR) via (CBORGroup (OCert crypto))

deriving instance Crypto crypto => Eq (OCert crypto)

deriving instance Crypto crypto => Show (OCert crypto)

instance Crypto crypto => NoUnexpectedThunks (OCert crypto)

instance
  (Crypto crypto) =>
  ToCBORGroup (OCert crypto)
  where
  toCBORGroup ocert =
    encodeVerKeyKES (ocertVkHot ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> encodeSignedDSIGN (ocertSigma ocert)
  encodedGroupSizeExpr size proxy =
    KES.encodedVerKeyKESSizeExpr (ocertVkHot <$> proxy)
      + encodedSizeExpr size ((toWord . ocertN) <$> proxy)
      + encodedSizeExpr size ((\(KESPeriod p) -> p) . ocertKESPeriod <$> proxy)
      + DSIGN.encodedSigDSIGNSizeExpr (((\(DSIGN.SignedDSIGN sig) -> sig) . ocertSigma) <$> proxy)
    where
      toWord :: Natural -> Word
      toWord = fromIntegral

  listLen _ = 4
  listLenBound _ = 4

instance
  (Crypto crypto) =>
  FromCBORGroup (OCert crypto)
  where
  fromCBORGroup =
    OCert
      <$> decodeVerKeyKES
      <*> fromCBOR
      <*> fromCBOR
      <*> decodeSignedDSIGN

kesPeriod :: SlotNo -> ShelleyBase KESPeriod
kesPeriod (SlotNo s) = asks slotsPerKESPeriod <&> \spkp ->
  if spkp == 0
    then error "kesPeriod: slots per KES period was set to zero"
    else KESPeriod . fromIntegral $ s `div` spkp
