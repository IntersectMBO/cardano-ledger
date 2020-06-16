{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Address.Bootstrap
  ( BootstrapWitness
      ( BootstrapWitness,
        bwKey,
        bwSig,
        bwChainCode,
        bwPadding
      ),
    KeyPadding (..),
    bootstrapWitKeyHash,
    unpackByronKey,
    getPadding,
  )
where

import Cardano.Binary
  ( Annotator,
    FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
    serialize',
    serializeEncoding',
    withSlice,
  )
import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN (rawSerialiseVerKeyDSIGN)
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Crypto.DSIGN.Ed25519 as Ed25519
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as WC
import Cardano.Prelude
  ( AllowThunksIn (..),
    ByteString,
    Generic,
    LByteString,
    NoUnexpectedThunks,
    Proxy (..),
    Word8,
  )
import qualified Data.ByteString.Lazy as LBS
import Data.Ord (comparing)
import Shelley.Spec.Ledger.Crypto (Crypto, DSIGN, HASH)
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash,
    KeyHash (..),
    KeyRole (..),
    SignedDSIGN,
    VKey (..),
  )
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.TxData (TxBody)

-- | This represents the data prepended/appended to a Byron verification key
-- when producing a key hash.
data KeyPadding = KeyPadding
  { paddingPrefix :: !ByteString,
    paddingSuffix :: !ByteString
  }
  deriving (Eq, Show, Generic)

instance NoUnexpectedThunks KeyPadding

newtype ChainCode = ChainCode ByteString
  deriving (Eq, Show, Generic)
  deriving newtype (NoUnexpectedThunks, ToCBOR, FromCBOR)

data BootstrapWitness crypto = BootstrapWitness'
  { bwKey' :: !(VKey 'AWitness crypto),
    bwSig' :: !(SignedDSIGN crypto (Hash crypto (TxBody crypto))),
    bwChainCode' :: !ChainCode,
    bwPadding' :: !KeyPadding,
    bwVKeyBytes :: LByteString
  }
  deriving (Eq, Generic, Show)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn
          '["bwVKeyBytes"]
          (BootstrapWitness crypto)

pattern BootstrapWitness ::
  Crypto crypto =>
  (VKey 'AWitness crypto) ->
  (SignedDSIGN crypto (Hash crypto (TxBody crypto))) ->
  ChainCode ->
  KeyPadding ->
  BootstrapWitness crypto
pattern BootstrapWitness {bwKey, bwSig, bwChainCode, bwPadding} <-
  BootstrapWitness' bwKey bwSig bwChainCode bwPadding _
  where
    BootstrapWitness key@(VKey k) sig cc pad =
      let keyBytes = LBS.fromStrict $ rawSerialiseVerKeyDSIGN k
       in BootstrapWitness' key sig cc pad keyBytes

{-# COMPLETE BootstrapWitness #-}

instance
  forall crypto.
  (Crypto crypto) =>
  Ord (BootstrapWitness crypto)
  where
  compare = comparing bootstrapWitKeyHash

instance Crypto crypto => ToCBOR (BootstrapWitness crypto) where
  toCBOR (BootstrapWitness key sig cc (KeyPadding prefix suffix)) =
    encodeListLen 5
      <> toCBOR key
      <> DSIGN.encodeSignedDSIGN sig
      <> toCBOR cc
      <> toCBOR prefix
      <> toCBOR suffix

instance Crypto crypto => FromCBOR (Annotator (BootstrapWitness crypto)) where
  fromCBOR = decodeRecordNamed "BootstrapWitness" (const 5) $ do
    (key, keyBytes) <- withSlice fromCBOR
    sig <- DSIGN.decodeSignedDSIGN
    cc <- fromCBOR
    prefix <- fromCBOR
    suffix <- fromCBOR
    pure $ (BootstrapWitness' key sig cc (KeyPadding prefix suffix)) <$> keyBytes

-- | Rebuild the addrRoot of the corresponding address.
bootstrapWitKeyHash ::
  forall crypto.
  Crypto crypto =>
  BootstrapWitness crypto ->
  KeyHash 'AWitness crypto
bootstrapWitKeyHash (BootstrapWitness' _ _ (ChainCode cc) (KeyPadding prefix suffix) keyBytes) =
  KeyHash . Hash.UnsafeHash . hash_crypto . hash_SHA3_256 $ bytes
  where
    bytes = prefix <> LBS.toStrict keyBytes <> cc <> suffix
    hash_SHA3_256 :: ByteString -> ByteString
    hash_SHA3_256 = Hash.digest (Proxy :: Proxy Hash.SHA3_256)
    hash_crypto :: ByteString -> ByteString
    hash_crypto = Hash.digest (Proxy :: Proxy (HASH crypto))

-- | This calculates the key padding of a Byron address by serializing the relevant parts.
-- | This only supports VKey addresses, not Redeem adresses
getPadding :: Byron.Address -> Maybe KeyPadding
getPadding (Byron.Address _ _ Byron.ATRedeem) = Nothing
getPadding (Byron.Address _ attributes Byron.ATVerKey) =
  -- The payload hashed to create an addrRoot consists of the following:
  -- 1: a token indicating a list of length 3
  -- 2: the addrType
  -- 3: the key
  -- 3a: token indicating list length 2
  -- 3b: token indicating address type (which will be a vkey address)
  -- 3c: a token indicating a bytestring of length 64
  -- 3d: public key bytes (32)
  -- 3e: chain code bytes (32)
  -- 4: the addrAttributes
  -- the prefix is all of the bytes before the bytes for the public key
  -- the suffix is all of the bytes after the bytes for the chain code
  let prefix =
        serializeEncoding' (encodeListLen 3) -- the surrounding 3-tuple
          <> serialize' Byron.ATVerKey
          <> serializeEncoding' (encodeListLen 2) -- the wrapper for the key
          <> serialize' (0 :: Word8) -- union tag for VKey address
          <> "\88\64" -- indicates what follows is a bytestring of length 64
      suffix = serialize' attributes
   in Just $ KeyPadding prefix suffix

unpackByronKey ::
  forall crypto.
  (DSIGN crypto ~ Ed25519DSIGN) =>
  Byron.VerificationKey ->
  (VKey 'AWitness crypto, ChainCode)
unpackByronKey
  ( Byron.VerificationKey
      (WC.XPub vkeyBytes (WC.ChainCode chainCodeBytes))
    ) = case DSIGN.rawDeserialiseVerKeyDSIGN vkeyBytes of
    -- This maybe is produced by a check that the length of the public key
    -- is the correct one. (32 bytes). If the XPub was constructed correctly,
    -- we already know that it has this length.
    Nothing -> error "this should be impossible!"
    Just vk -> (VKey vk, ChainCode chainCodeBytes)
