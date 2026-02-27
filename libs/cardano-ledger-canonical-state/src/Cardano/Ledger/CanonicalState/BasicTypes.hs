{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.BasicTypes (
  OnChain (..),
  DecodeOnChain (..),
  mkOnChain,
  CanonicalCoin (..),
  CanonicalExUnits (..),
  mkCanonicalExUnits,
  fromCanonicalExUnits,
  CanonicalVRFVerKeyHash (..),
  mkCanonicalVRFVerKeyHash,
  fromCanonicalVRFVerKeyHash,
  CanonicalRewardAccount (..),
  mkCanonicalRewardAccount,
  fromCanonicalRewardAccount,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  EpochInterval,
  NonNegativeInterval,
  ProtVer (..),
  EpochNo (..),
  Network (..),
  SlotNo (..),
  StrictMaybe (..),
  UnitInterval,
  Url (..),
  textToUrl,
  urlToText,
 )
import Cardano.Ledger.CanonicalState.LedgerCBOR
import Cardano.Ledger.CanonicalState.Namespace (Era, NamespaceEra)
import Cardano.Ledger.Binary (encCBOR, serialize', EncCBOR)
import Cardano.Ledger.Core (eraProtVerLow, AccountAddress (..), AccountId (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (HASH, Hash, KeyHash (..), ScriptHash (..))
import qualified Cardano.Ledger.Hashes as H
import Cardano.Ledger.Plutus.ExUnits (ExUnits (..), ExUnits' (..))
import Cardano.SCLS.CBOR.Canonical (CanonicalDecoder)
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
  peekTokenType,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.Versioned
import qualified Codec.CBOR.Decoding as D
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Base16 as Base16
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits

-- | Wrapper type that tells that the type is the type that is kept on-chain
-- for such types we want to keep exactly the same encoding as on the wire.
--
-- We still tag the type with an original one to be able to distinguish between
-- them.
data OnChain (a :: Type) = OnChain {getValue :: !a, getWireEncoding :: !BS.ByteString}
  deriving stock (Generic)

mkOnChain :: forall era a . (Era era, EncCBOR a) => a -> OnChain a
mkOnChain x = OnChain x $! serialize' (eraProtVerLow @era) (encCBOR x)

instance Eq a => Eq (OnChain a) where
  (OnChain _ bs1) == (OnChain _ bs2) = bs1 == bs2

instance Ord a => Ord (OnChain a) where
  compare (OnChain _ bs1) (OnChain _ bs2) = compare bs1 bs2

instance Show a => Show (OnChain a) where
  show (OnChain a b) = show (Base16.encode b) ++ ":" ++ show a

instance ToCanonicalCBOR v (OnChain a) where
  toCanonicalCBOR v (OnChain _ bs) = toCanonicalCBOR v bs

instance DecodeOnChain v a => FromCanonicalCBOR v (OnChain a) where
  fromCanonicalCBOR = do
    Versioned bs <- fromCanonicalCBOR
    a <- decodeOnChain @v bs
    return $ Versioned (OnChain a bs)

-- | Helper types to encode on-chain types, it's used so
-- it would be possible to pass input bytestring to
-- `toPlainDecoder`.
class DecodeOnChain (v :: Symbol) (a :: Type) where
  decodeOnChain :: BS.ByteString -> CanonicalDecoder s a

-- | Wrapper for the coin type.
--
-- Despite the fact that Coin is on-chain type, we do not want to use
-- 'OnChain' wrapper for it. Because it's expected that if we keep chain
-- structure like transaction in canonical state, then we should keep entire
-- structure there and keep that as a whole, like 'UTxOut'.
newtype CanonicalCoin = CanonicalCoin {unCoin :: CompactForm Coin}
  deriving (Eq, Ord, Show, Generic)

instance FromCanonicalCBOR v CanonicalCoin where
  fromCanonicalCBOR = fmap (CanonicalCoin . CompactCoin) <$> fromCanonicalCBOR

instance ToCanonicalCBOR v CanonicalCoin where
  toCanonicalCBOR v (CanonicalCoin (CompactCoin c)) = toCanonicalCBOR v c

instance ToCanonicalCBOR v a => ToCanonicalCBOR v (StrictMaybe a) where
  toCanonicalCBOR v SNothing = toCanonicalCBOR v ()
  toCanonicalCBOR v (SJust x) = toCanonicalCBOR v x

instance FromCanonicalCBOR v a => FromCanonicalCBOR v (StrictMaybe a) where
  fromCanonicalCBOR = do
    mt <- peekTokenType
    case mt of
      D.TypeNull -> do
        Versioned () <- fromCanonicalCBOR
        pure (Versioned SNothing)
      _ -> fmap SJust <$> fromCanonicalCBOR

deriving via
  LedgerCBOR v EpochNo
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v EpochNo

deriving via
  LedgerCBOR v EpochNo
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v EpochNo

deriving via
  LedgerCBOR v Anchor
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v Anchor

deriving via
  LedgerCBOR v Anchor
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v Anchor

deriving via
  LedgerCBOR v ScriptHash
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v ScriptHash

deriving via
  LedgerCBOR v ScriptHash
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v ScriptHash

instance ToCanonicalCBOR v (H.Hash a b) where
  toCanonicalCBOR v h = toCanonicalCBOR v (Hash.hashToBytes h)

instance H.HashAlgorithm a => FromCanonicalCBOR v (H.Hash a b) where
  fromCanonicalCBOR = do
    Versioned bytes <- fromCanonicalCBOR
    case Hash.hashFromBytesShort bytes of
      Just h -> return (Versioned h)
      Nothing -> fail "Invalid hash bytes"

deriving newtype instance ToCanonicalCBOR v SlotNo

deriving newtype instance FromCanonicalCBOR v SlotNo

deriving via
  LedgerCBOR v (H.KeyHash kr)
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (H.KeyHash kr)

deriving via
  LedgerCBOR v (H.KeyHash kr)
  instance
    (Era era, NamespaceEra v ~ era, Typeable kr) => FromCanonicalCBOR v (H.KeyHash kr)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v (Credential kr) where
  toCanonicalCBOR v (ScriptHashObj sh) = toCanonicalCBOR v (0 :: Word8, sh)
  toCanonicalCBOR v (KeyHashObj kh) = toCanonicalCBOR v (1 :: Word8, kh)

instance (Era era, NamespaceEra v ~ era, Typeable kr) => FromCanonicalCBOR v (Credential kr) where
  fromCanonicalCBOR = do
    decodeListLenCanonicalOf 2
    Versioned (tag :: Word8) <- fromCanonicalCBOR
    case tag of
      0 -> fmap ScriptHashObj <$> fromCanonicalCBOR @v
      1 -> fmap KeyHashObj <$> fromCanonicalCBOR @v
      _ -> fail "Invalid Credential tag"

deriving via
  LedgerCBOR v UnitInterval
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v UnitInterval

deriving via
  LedgerCBOR v UnitInterval
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v UnitInterval

deriving via
  LedgerCBOR v NonNegativeInterval
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v NonNegativeInterval

deriving via
  LedgerCBOR v NonNegativeInterval
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v NonNegativeInterval

deriving via
  LedgerCBOR v ProtVer
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v ProtVer

deriving via
  LedgerCBOR v ProtVer
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v ProtVer

deriving via
  LedgerCBOR v EpochInterval
  instance
    (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v EpochInterval

deriving via
  LedgerCBOR v EpochInterval
  instance
    (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v EpochInterval

data CanonicalExUnits = CanonicalExUnits
  { exUnitsMem' :: !Natural
  , exUnitsSteps' :: !Natural
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalExUnits where
  toCanonicalCBOR v CanonicalExUnits {..} = toCanonicalCBOR v (exUnitsMem', exUnitsSteps')

instance FromCanonicalCBOR v CanonicalExUnits where
  fromCanonicalCBOR = do
    Versioned (exUnitsMem', exUnitsSteps') <- fromCanonicalCBOR @v
    return $ Versioned CanonicalExUnits {..}

mkCanonicalExUnits :: ExUnits -> CanonicalExUnits
mkCanonicalExUnits (unWrapExUnits -> ExUnits' {..}) = CanonicalExUnits {..}

fromCanonicalExUnits :: CanonicalExUnits -> ExUnits
fromCanonicalExUnits CanonicalExUnits {..} = WrapExUnits ExUnits' {..}

newtype CanonicalVRFVerKeyHash (kr :: H.KeyRoleVRF) = CanonicalVRFVerKeyHash {unCanonicalVRFVerKeyHash :: Hash HASH H.KeyRoleVRF}
  deriving (Eq, Ord, Show, Generic)

mkCanonicalVRFVerKeyHash :: H.VRFVerKeyHash kr -> CanonicalVRFVerKeyHash kr
mkCanonicalVRFVerKeyHash (H.VRFVerKeyHash k) = CanonicalVRFVerKeyHash k

fromCanonicalVRFVerKeyHash :: CanonicalVRFVerKeyHash kr -> H.VRFVerKeyHash kr
fromCanonicalVRFVerKeyHash (CanonicalVRFVerKeyHash k) = H.VRFVerKeyHash k

instance (NamespaceEra v ~ era, Era era) => ToCanonicalCBOR v (CanonicalVRFVerKeyHash kr) where
  toCanonicalCBOR v (CanonicalVRFVerKeyHash vrf) = toCanonicalCBOR v vrf

instance (NamespaceEra v ~ era, Era era) => FromCanonicalCBOR v (CanonicalVRFVerKeyHash kr) where
  fromCanonicalCBOR = fmap CanonicalVRFVerKeyHash <$> fromCanonicalCBOR

instance ToCanonicalCBOR v Url where
  toCanonicalCBOR v u = toCanonicalCBOR v (urlToText u)

instance FromCanonicalCBOR v Url where
  fromCanonicalCBOR = do
    Versioned t <- fromCanonicalCBOR
    case textToUrl 128 t of
      Just url -> return $ Versioned url
      Nothing -> fail "Invalid URL"

data CanonicalRewardAccount = CanonicalRewardAccount
  { raNetwork :: !Network
  , raCredential :: !(Credential H.Staking)
  }
  deriving (Eq, Show, Ord, Generic)

instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v CanonicalRewardAccount where
  toCanonicalCBOR v = toCanonicalCBOR v . fromCanonicalRewardAccount

instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v CanonicalRewardAccount where
  fromCanonicalCBOR = fmap mkCanonicalRewardAccount <$> fromCanonicalCBOR @v

mkCanonicalRewardAccount :: AccountAddress -> CanonicalRewardAccount
mkCanonicalRewardAccount AccountAddress {aaId = AccountId raCredential, aaNetworkId} =
  CanonicalRewardAccount
    { raCredential = raCredential
    , raNetwork = aaNetworkId
    }

fromCanonicalRewardAccount :: CanonicalRewardAccount -> AccountAddress
fromCanonicalRewardAccount CanonicalRewardAccount {..} =
  AccountAddress
    { aaId = AccountId raCredential
    , aaNetworkId = raNetwork
    }

deriving via LedgerCBOR v AccountAddress instance (Era era, NamespaceEra v ~ era) => ToCanonicalCBOR v AccountAddress

deriving via LedgerCBOR v AccountAddress instance (Era era, NamespaceEra v ~ era) => FromCanonicalCBOR v AccountAddress
