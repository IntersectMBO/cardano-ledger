{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Chain.Common.AddrSpendingData
  ( AddrSpendingData(..)
  , AddrType(..)
  , addrSpendingDataToType
  )
where

import Cardano.Prelude

import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  (Bi(..), Case(..), DecoderError(..), encodeListLen, enforceSize, szCases)
import Cardano.Crypto.Signing (PublicKey, RedeemPublicKey)


-- | Data which is bound to an address and must be revealed in order to spend
--   lovelace belonging to this address.
data AddrSpendingData
  = PubKeyASD !PublicKey
  -- ^ Funds can be spent by revealing a 'PublicKey' and providing a valid
  --   signature
  | RedeemASD !RedeemPublicKey
  -- ^ Funds can be spent by revealing a 'RedeemPublicKey' and providing a
  --   valid signature
  deriving (Eq, Generic, Show)
  deriving anyclass NFData

instance B.Buildable AddrSpendingData where
  build = \case
    PubKeyASD pk  -> bprint ("PubKeyASD " . build) pk
    RedeemASD rpk -> bprint ("RedeemASD " . build) rpk

-- Tag 1 was previously used for scripts, but never appeared on the chain
instance Bi AddrSpendingData where
  encode = \case
    PubKeyASD pk -> encodeListLen 2 <> encode (0 :: Word8) <> encode pk
    RedeemASD redeemPK ->
      encodeListLen 2 <> encode (2 :: Word8) <> encode redeemPK

  decode = do
    enforceSize "AddrSpendingData" 2
    decode @Word8 >>= \case
      0   -> PubKeyASD <$> decode
      2   -> RedeemASD <$> decode
      tag -> cborError $ DecoderErrorUnknownTag "AddrSpendingData" tag

  encodedSizeExpr size _ = szCases
    [ Case "PubKeyASD" $ size $ Proxy @(Word8, PublicKey)
    , Case "RedeemASD" $ size $ Proxy @(Word8, RedeemPublicKey)
    ]

-- | Type of an address. It corresponds to constructors of 'AddrSpendingData'.
--   It's separated, because 'Address' doesn't store 'AddrSpendingData', but we
--   want to know its type.
data AddrType
  = ATPubKey
  | ATRedeem
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass NFData

-- Tag 1 was previously used for scripts, but never appeared on the chain
instance Bi AddrType where
  encode = encode @Word8 . \case
    ATPubKey -> 0
    ATRedeem -> 2

  decode = decode @Word8 >>= \case
    0   -> pure ATPubKey
    2   -> pure ATRedeem
    tag -> cborError $ DecoderErrorUnknownTag "AddrType" tag

  encodedSizeExpr size _ = encodedSizeExpr size (Proxy @Word8)

instance HeapWords AddrType where
  heapWords = \case
    ATPubKey    -> 0
    ATRedeem    -> 0

-- | Convert 'AddrSpendingData' to the corresponding 'AddrType'
addrSpendingDataToType :: AddrSpendingData -> AddrType
addrSpendingDataToType = \case
  PubKeyASD{} -> ATPubKey
  RedeemASD{} -> ATRedeem
