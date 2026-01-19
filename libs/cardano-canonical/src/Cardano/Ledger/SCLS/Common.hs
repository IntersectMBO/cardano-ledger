{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.SCLS.Common
  ( module Cardano.Ledger.SCLS.BaseTypes
  , CanonicalCoin (..)
  , IsCanonicalCoin (..)
  , CanonicalCredential (..)
  , fromCanonicalCredential
  , mkCanonicalCredential
  , CanonicalRewardAccount (..)
  , fromCanonicalRewardAccount
  , mkCanonicalRewardAccount
  , CanonicalVRFVerKeyHash (..)
  , fromCanonicalVRFVerKeyHash
  , mkCanonicalVRFVerKeyHash
  , Nonce (..)
  , CoinPerByte (..)
  ) where

import Cardano.SCLS.CDDL ()
import Cardano.Ledger.SCLS.BaseTypes
import Cardano.Ledger.Coin (Coin(..), CompactForm (..), CoinPerByte (..))
import Cardano.Ledger.Plutus.ExUnits (Prices)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Hashes as H
import Cardano.SCLS.Versioned (Versioned (..))
import Cardano.Ledger.Address (RewardAccount (..))
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
   -- peekTokenType,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import GHC.Generics (Generic)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (
  HASH,
  Hash,
  KeyHash,
  KeyRoleVRF,
  ScriptHash,
  Staking,
  VRFVerKeyHash (..),
 )
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Cardano.Ledger.SCLS.LedgerCBOR (LedgerCBOR (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq

-- | Wrapper for the coin type
newtype CanonicalCoin = CanonicalCoin {unCoin :: Integer}
  deriving (Eq, Ord, Show, Generic)

-- | We introduce type class here because Coin can be created from multiple types.
class IsCanonicalCoin a where
  mkCanonicalCoin :: a -> CanonicalCoin
  fromCanonicalCoin :: CanonicalCoin -> a

instance IsCanonicalCoin CanonicalCoin where
  mkCanonicalCoin = id
  fromCanonicalCoin = id

instance IsCanonicalCoin Coin where
  mkCanonicalCoin Coin{..} = CanonicalCoin{..}
  fromCanonicalCoin (CanonicalCoin i) = Coin i

instance IsCanonicalCoin (CompactForm Coin) where
  mkCanonicalCoin (CompactCoin ci) = CanonicalCoin (fromIntegral ci)
  fromCanonicalCoin (CanonicalCoin ci) = CompactCoin (fromIntegral ci)

instance ToCanonicalCBOR v CanonicalCoin where
  toCanonicalCBOR v (CanonicalCoin ci) = toCanonicalCBOR v ci

instance FromCanonicalCBOR v CanonicalCoin where
  fromCanonicalCBOR = fmap CanonicalCoin <$> fromCanonicalCBOR

-- | Credential key, it does not keep the role around, because the role is
-- created anyway as we serialize the value.
data CanonicalCredential kr
  = CanonicalScriptHashObj !ScriptHash
  | CanonicalKeyHashObj !(KeyHash kr)
  deriving (Eq, Show, Ord, Generic)

instance ToCanonicalCBOR v (CanonicalCredential kr) where
  toCanonicalCBOR v (CanonicalScriptHashObj sh) = toCanonicalCBOR v (0 :: Word8, sh)
  toCanonicalCBOR v (CanonicalKeyHashObj kh) = toCanonicalCBOR v (1 :: Word8, kh)

instance Typeable kr => FromCanonicalCBOR v (CanonicalCredential kr) where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap CanonicalScriptHashObj <$> fromCanonicalCBOR
      1 -> fmap CanonicalKeyHashObj <$> fromCanonicalCBOR
      _ -> fail "Invalid Credential tag"

mkCanonicalCredential :: Credential kr -> CanonicalCredential kr
mkCanonicalCredential (ScriptHashObj sh) = CanonicalScriptHashObj sh
mkCanonicalCredential (KeyHashObj kh) = CanonicalKeyHashObj kh

fromCanonicalCredential :: CanonicalCredential kr -> Credential kr
fromCanonicalCredential (CanonicalScriptHashObj sh) = ScriptHashObj sh
fromCanonicalCredential (CanonicalKeyHashObj sh) = KeyHashObj sh

instance ToCanonicalCBOR v (Credential kr) where
  toCanonicalCBOR v (ScriptHashObj sh) = toCanonicalCBOR v (0 :: Word8, sh)
  toCanonicalCBOR v (KeyHashObj kh) = toCanonicalCBOR v (1 :: Word8, kh)

instance Typeable kr => FromCanonicalCBOR v (Credential kr) where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap ScriptHashObj <$> fromCanonicalCBOR
      1 -> fmap KeyHashObj <$> fromCanonicalCBOR
      _ -> fail "Invalid Credential tag"

deriving via LedgerCBOR v ScriptHash instance ToCanonicalCBOR v ScriptHash

deriving via LedgerCBOR v ScriptHash instance FromCanonicalCBOR v ScriptHash

deriving via LedgerCBOR v (KeyHash kr) instance ToCanonicalCBOR v (KeyHash kr)

deriving via LedgerCBOR v (KeyHash kr) instance Typeable kr => FromCanonicalCBOR v (KeyHash kr)


data CanonicalRewardAccount = CanonicalRewardAccount
  { raNetwork :: !Network
  , raCredential :: !(CanonicalCredential Staking)
  }
  deriving (Eq, Show, Ord, Generic)

instance ToCanonicalCBOR v CanonicalRewardAccount where
  toCanonicalCBOR v = toCanonicalCBOR v . fromCanonicalRewardAccount

instance FromCanonicalCBOR v CanonicalRewardAccount where
  fromCanonicalCBOR = fmap mkCanonicalRewardAccount <$> fromCanonicalCBOR

mkCanonicalRewardAccount :: RewardAccount -> CanonicalRewardAccount
mkCanonicalRewardAccount RewardAccount {..} =
  CanonicalRewardAccount
    { raCredential = mkCanonicalCredential raCredential
    , ..
    }

fromCanonicalRewardAccount :: CanonicalRewardAccount -> RewardAccount
fromCanonicalRewardAccount CanonicalRewardAccount {..} =
  RewardAccount
    { raCredential = fromCanonicalCredential raCredential
    , ..
    }

deriving via LedgerCBOR v RewardAccount instance ToCanonicalCBOR v RewardAccount

deriving via LedgerCBOR v RewardAccount instance FromCanonicalCBOR v RewardAccount

newtype CanonicalVRFVerKeyHash (kr :: KeyRoleVRF) = CanonicalVRFVerKeyHash {unCanonicalVRFVerKeyHash :: Hash HASH KeyRoleVRF}
  deriving (Eq, Ord, Show, Generic)

mkCanonicalVRFVerKeyHash :: VRFVerKeyHash kr -> CanonicalVRFVerKeyHash kr
mkCanonicalVRFVerKeyHash (VRFVerKeyHash k) = CanonicalVRFVerKeyHash k

fromCanonicalVRFVerKeyHash :: CanonicalVRFVerKeyHash kr -> VRFVerKeyHash kr
fromCanonicalVRFVerKeyHash (CanonicalVRFVerKeyHash k) = VRFVerKeyHash k

instance ToCanonicalCBOR v (CanonicalVRFVerKeyHash kr) where
  toCanonicalCBOR v (CanonicalVRFVerKeyHash vrf) = toCanonicalCBOR v vrf

instance FromCanonicalCBOR v (CanonicalVRFVerKeyHash kr) where
  fromCanonicalCBOR = fmap CanonicalVRFVerKeyHash <$> fromCanonicalCBOR

instance ToCanonicalCBOR v (H.Hash a b) where
  toCanonicalCBOR v h = toCanonicalCBOR v (Hash.hashToBytes h)

instance H.HashAlgorithm a => FromCanonicalCBOR v (H.Hash a b) where
  fromCanonicalCBOR = do
    Versioned bytes <- fromCanonicalCBOR
    case Hash.hashFromBytesShort bytes of
      Just h -> return (Versioned h)
      Nothing -> fail "Invalid hash bytes"

deriving via LedgerCBOR v Prices instance ToCanonicalCBOR v Prices

deriving via LedgerCBOR v Prices instance FromCanonicalCBOR v Prices

instance FromCanonicalCBOR v a => FromCanonicalCBOR v (StrictSeq a) where
  fromCanonicalCBOR = do
    Versioned (l :: [a]) <- fromCanonicalCBOR @v
    pure $ Versioned $ StrictSeq.fromList l