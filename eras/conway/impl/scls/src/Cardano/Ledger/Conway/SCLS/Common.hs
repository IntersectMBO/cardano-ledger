{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Common namespace utilities and types for SCLS export.
module Cardano.Ledger.Conway.SCLS.Common (
  CanonicalCredential (..),
  fromCanonicalCredential,
  mkCanonicalCredential,
) where

import Cardano.Ledger.BaseTypes (Anchor, EpochNo, NonNegativeInterval, ProtVer, UnitInterval)
import Cardano.Ledger.Coin (Coin, CompactForm)
import qualified Cardano.Ledger.Coin as Coin
import Cardano.Ledger.Conway.SCLS.LedgerCBOR (LedgerCBOR (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (KeyHash, ScriptHash, VRFVerKeyHash)
import Cardano.Ledger.Plutus.ExUnits (Prices)
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonicalOf,
  peekTokenType,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import Cardano.SCLS.CDDL ()
import Cardano.SCLS.Versioned (Versioned (..))
import Cardano.Slotting.Slot (EpochInterval, SlotNo (..))
import qualified Codec.CBOR.Decoding as D
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)

instance ToCanonicalCBOR v (CompactForm Coin) where
  toCanonicalCBOR v (Coin.CompactCoin ci) = toCanonicalCBOR v ci

instance FromCanonicalCBOR v (CompactForm Coin) where
  fromCanonicalCBOR = fmap Coin.CompactCoin <$> fromCanonicalCBOR

instance ToCanonicalCBOR v Coin where
  toCanonicalCBOR v (Coin.Coin ci) = toCanonicalCBOR v ci

instance FromCanonicalCBOR v Coin where
  fromCanonicalCBOR = fmap Coin.Coin <$> fromCanonicalCBOR

-- | Credential key, it does not keep the role around, because the role is
-- created anyway as we serialize the value.
data CanonicalCredential kr
  = CanonicalScriptHashObj !ScriptHash
  | CanonicalKeyHashObj !(KeyHash kr)
  deriving (Eq, Show, Ord, Generic)

mkCanonicalCredential :: Credential kr -> CanonicalCredential kr
mkCanonicalCredential (ScriptHashObj sh) = CanonicalScriptHashObj sh
mkCanonicalCredential (KeyHashObj kh) = CanonicalKeyHashObj kh

fromCanonicalCredential :: CanonicalCredential kr -> Credential kr
fromCanonicalCredential (CanonicalScriptHashObj sh) = ScriptHashObj sh
fromCanonicalCredential (CanonicalKeyHashObj sh) = KeyHashObj sh

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

deriving via LedgerCBOR v (VRFVerKeyHash kr) instance ToCanonicalCBOR v (VRFVerKeyHash kr)

deriving via
  LedgerCBOR v (VRFVerKeyHash kr)
  instance
    Typeable kr => FromCanonicalCBOR v (VRFVerKeyHash kr)

deriving via LedgerCBOR v NonNegativeInterval instance ToCanonicalCBOR v NonNegativeInterval

deriving via LedgerCBOR v NonNegativeInterval instance FromCanonicalCBOR v NonNegativeInterval

deriving via LedgerCBOR v UnitInterval instance ToCanonicalCBOR v UnitInterval

deriving via LedgerCBOR v UnitInterval instance FromCanonicalCBOR v UnitInterval

deriving via LedgerCBOR v Prices instance ToCanonicalCBOR v Prices

deriving via LedgerCBOR v Prices instance FromCanonicalCBOR v Prices

deriving via LedgerCBOR v EpochInterval instance ToCanonicalCBOR v EpochInterval

deriving via LedgerCBOR v EpochInterval instance FromCanonicalCBOR v EpochInterval

deriving via LedgerCBOR v ProtVer instance ToCanonicalCBOR v ProtVer

deriving via LedgerCBOR v ProtVer instance FromCanonicalCBOR v ProtVer

deriving via LedgerCBOR v Anchor instance ToCanonicalCBOR v Anchor

deriving via LedgerCBOR v Anchor instance FromCanonicalCBOR v Anchor

deriving via LedgerCBOR v EpochNo instance ToCanonicalCBOR v EpochNo

deriving via LedgerCBOR v EpochNo instance FromCanonicalCBOR v EpochNo

deriving newtype instance ToCanonicalCBOR v SlotNo

deriving newtype instance FromCanonicalCBOR v SlotNo

instance FromCanonicalCBOR v a => FromCanonicalCBOR v (StrictSeq a) where
  fromCanonicalCBOR = do
    Versioned (l :: [a]) <- fromCanonicalCBOR @v
    pure $ Versioned $ StrictSeq.fromList l
