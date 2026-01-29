{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.CanonicalState.Namespace.Pots.V0 (
  PotsIn (..),
  PotsOut (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.CanonicalState.BasicTypes (CanonicalCoin (..))
import Cardano.SCLS.CBOR.Canonical.Decoder (FromCanonicalCBOR (..), decodeMapLenCanonicalOf)
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey (IsKey (..))
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Data.MemPack.ByteOrdered (packWord64beM, unpackBigEndianM)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)

instance KnownNamespace "pots/v0" where
  type NamespaceKey "pots/v0" = PotsIn
  type NamespaceEntry "pots/v0" = PotsOut

newtype PotsIn = PotsIn EpochNo
  deriving (Eq, Ord, Show)

instance IsKey PotsIn where
  keySize = namespaceKeySize @"pots/v0"
  packKeyM (PotsIn (EpochNo epochNo)) = do
    packWord64beM epochNo
  unpackKeyM = do
    epochNo <- unpackBigEndianM
    return $ PotsIn (EpochNo epochNo)

data PotsOut = PotsOut
  { poFee :: !CanonicalCoin
  , poDeposit :: !CanonicalCoin
  , poDonation :: !CanonicalCoin
  , poReserves :: !CanonicalCoin
  , poTreasury :: !CanonicalCoin
  }
  deriving (Eq, Show)
  deriving (Generic)

instance ToCanonicalCBOR "pots/v0" PotsOut where
  toCanonicalCBOR v PotsOut {..} =
    encodeAsMap
      [ mkEncodablePair v ("fee" :: Text) poFee
      , mkEncodablePair v ("deposit" :: Text) poDeposit
      , mkEncodablePair v ("donation" :: Text) poDonation
      , mkEncodablePair v ("reserves" :: Text) poReserves
      , mkEncodablePair v ("treasury" :: Text) poTreasury
      ]

instance FromCanonicalCBOR "pots/v0" PotsOut where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 5
    Versioned ("fee" :: Text) <- fromCanonicalCBOR
    Versioned poFee <- fromCanonicalCBOR
    Versioned ("deposit" :: Text) <- fromCanonicalCBOR
    Versioned poDeposit <- fromCanonicalCBOR
    Versioned ("donation" :: Text) <- fromCanonicalCBOR
    Versioned poDonation <- fromCanonicalCBOR
    Versioned ("reserves" :: Text) <- fromCanonicalCBOR
    Versioned poReserves <- fromCanonicalCBOR
    Versioned ("treasury" :: Text) <- fromCanonicalCBOR
    Versioned poTreasury <- fromCanonicalCBOR
    pure (Versioned PotsOut {..})

instance CanonicalCBOREntryEncoder "pots/v0" PotsOut where
  encodeEntry n = toCanonicalCBOR (Proxy @"pots/v0") n

instance CanonicalCBOREntryDecoder "pots/v0" PotsOut where
  decodeEntry = fromCanonicalCBOR
