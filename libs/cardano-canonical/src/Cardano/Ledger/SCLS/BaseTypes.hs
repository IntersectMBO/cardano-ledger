-- | Canonical definitions for the base types.
--
-- As per @adr-010@ we just re-use existing base types without an wrappers.
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Ledger.SCLS.BaseTypes
  ( module Export
  , IPv4
  , IPv6
  ) where

-- Some rules for supporting the module, if the type is re-exported from the BaseTypes
-- module then we should import it from there. It would allow us to check the simple rule,
-- that we may not introduce a wrapper types for the BaseTypes.

import Cardano.Ledger.BaseTypes as Export
  ( Anchor(..),
    EpochNo(..),
    EpochInterval(..),
    NonNegativeInterval,
    Port,
    ProtVer(..),
    SlotNo(..),
    UnitInterval,
    Url(..),
    StrictMaybe(..),
    DnsName(..),
  )
import Cardano.Ledger.BaseTypes (
  textToDns,
  textToUrl,
 )
import Cardano.Ledger.SCLS.LedgerCBOR (LedgerCBOR (..))
import Cardano.SCLS.CBOR.Canonical (
  assumeCanonicalDecoder,
  assumeCanonicalEncoding,
 )
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  -- decodeListLenCanonicalOf,
  peekTokenType,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..))
import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import Cardano.SCLS.Versioned (Versioned (..))
import Data.IP (IPv4, IPv6)
import GHC.Natural (Natural)

deriving via LedgerCBOR v Anchor instance ToCanonicalCBOR v Anchor

deriving via LedgerCBOR v Anchor instance FromCanonicalCBOR v Anchor

instance ToCanonicalCBOR v DnsName where
  toCanonicalCBOR v = toCanonicalCBOR v . dnsToText

instance FromCanonicalCBOR v DnsName where
  fromCanonicalCBOR = do
    Versioned t <- fromCanonicalCBOR
    case textToDns 128 t of
      Just dns -> return $ Versioned dns
      Nothing -> fail "FromCanonicalCBOR<DsnName>: DNS name is too long"

deriving via LedgerCBOR v EpochInterval instance ToCanonicalCBOR v EpochInterval

deriving via LedgerCBOR v EpochInterval instance FromCanonicalCBOR v EpochInterval

deriving via LedgerCBOR v EpochNo instance ToCanonicalCBOR v EpochNo

deriving via LedgerCBOR v EpochNo instance FromCanonicalCBOR v EpochNo

deriving via LedgerCBOR v IPv4 instance ToCanonicalCBOR v IPv4

deriving via LedgerCBOR v IPv4 instance FromCanonicalCBOR v IPv4

deriving via LedgerCBOR v IPv6 instance ToCanonicalCBOR v IPv6

deriving via LedgerCBOR v IPv6 instance FromCanonicalCBOR v IPv6

deriving via LedgerCBOR v NonNegativeInterval instance ToCanonicalCBOR v NonNegativeInterval

deriving via LedgerCBOR v NonNegativeInterval instance FromCanonicalCBOR v NonNegativeInterval

deriving via LedgerCBOR v Port instance ToCanonicalCBOR v Port

deriving via LedgerCBOR v Port instance FromCanonicalCBOR v Port

deriving via LedgerCBOR v ProtVer instance ToCanonicalCBOR v ProtVer

deriving via LedgerCBOR v ProtVer instance FromCanonicalCBOR v ProtVer

deriving newtype instance ToCanonicalCBOR v SlotNo

deriving newtype instance FromCanonicalCBOR v SlotNo

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

deriving via LedgerCBOR v UnitInterval instance ToCanonicalCBOR v UnitInterval

deriving via LedgerCBOR v UnitInterval instance FromCanonicalCBOR v UnitInterval

instance ToCanonicalCBOR v Url where
  toCanonicalCBOR v u = toCanonicalCBOR v (urlToText u)

instance FromCanonicalCBOR v Url where
  fromCanonicalCBOR = do
    Versioned t <- fromCanonicalCBOR
    case textToUrl 128 t of
      Just url -> return $ Versioned url
      Nothing -> fail "Invalid URL"


-- TODO: remove

instance FromCanonicalCBOR v Natural where
  fromCanonicalCBOR = assumeCanonicalDecoder $ Versioned @v . fromIntegral <$> D.decodeIntegerCanonical

instance ToCanonicalCBOR v Natural where
  toCanonicalCBOR _v n = assumeCanonicalEncoding $ E.encodeInteger (fromIntegral n)