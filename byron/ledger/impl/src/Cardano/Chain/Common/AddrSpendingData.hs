{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.Common.AddrSpendingData
  ( AddrSpendingData (..),
    AddrType (..),
    addrSpendingDataToType,
  )
where

import Cardano.Binary
  ( Case (..),
    DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    decodeListLenCanonical,
    decodeWord8Canonical,
    encodeListLen,
    matchSize,
    szCases,
  )
import Cardano.Crypto.Signing (RedeemVerificationKey, VerificationKey)
import Cardano.Prelude
import Data.Aeson (ToJSON)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | Data which is bound to an address and must be revealed in order to spend
--   lovelace belonging to this address.
data AddrSpendingData
  = -- | Funds can be spent by revealing a 'VerificationKey' and providing a valid
    --   signature
    VerKeyASD !VerificationKey
  | -- | Funds can be spent by revealing a 'RedeemVerificationKey' and providing a
    --   valid signature
    RedeemASD !RedeemVerificationKey
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

instance B.Buildable AddrSpendingData where
  build = \case
    VerKeyASD vk -> bprint ("VerKeyASD " . build) vk
    RedeemASD rvk -> bprint ("RedeemASD " . build) rvk

-- Tag 1 was previously used for scripts, but never appeared on the chain
instance ToCBOR AddrSpendingData where
  toCBOR = \case
    VerKeyASD vk -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR vk
    RedeemASD redeemVK ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR redeemVK

  encodedSizeExpr size _ =
    szCases
      [ Case "VerKeyASD" $ size $ Proxy @(Word8, VerificationKey),
        Case "RedeemASD" $ size $ Proxy @(Word8, RedeemVerificationKey)
      ]

instance FromCBOR AddrSpendingData where
  fromCBOR = do
    len <- decodeListLenCanonical
    matchSize "AddrSpendingData" 2 len
    decodeWord8Canonical >>= \case
      0 -> VerKeyASD <$> fromCBOR
      2 -> RedeemASD <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "AddrSpendingData" tag

-- | Type of an address. It corresponds to constructors of 'AddrSpendingData'.
--   It's separated, because 'Address' doesn't store 'AddrSpendingData', but we
--   want to know its type.
data AddrType
  = ATVerKey
  | ATRedeem
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData, NoThunks)

-- Used for debugging purposes only
instance ToJSON AddrType

-- Tag 1 was previously used for scripts, but never appeared on the chain
instance ToCBOR AddrType where
  toCBOR =
    toCBOR @Word8 . \case
      ATVerKey -> 0
      ATRedeem -> 2

  encodedSizeExpr size _ = encodedSizeExpr size (Proxy @Word8)

instance FromCBOR AddrType where
  fromCBOR =
    decodeWord8Canonical >>= \case
      0 -> pure ATVerKey
      2 -> pure ATRedeem
      tag -> cborError $ DecoderErrorUnknownTag "AddrType" tag

instance HeapWords AddrType where
  heapWords = \case
    ATVerKey -> 0
    ATRedeem -> 0

-- | Convert 'AddrSpendingData' to the corresponding 'AddrType'
addrSpendingDataToType :: AddrSpendingData -> AddrType
addrSpendingDataToType = \case
  VerKeyASD {} -> ATVerKey
  RedeemASD {} -> ATRedeem
