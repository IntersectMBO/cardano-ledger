{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
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
    ChainCode (..),
    bootstrapWitKeyHash,
    unpackByronVKey,
    byronAddressPadding,
    byronVerKeyAddressPadding,
    makeBootstrapWitness,
  )
where

import Cardano.Binary
  ( Annotator,
    FromCBOR (..),
    ToCBOR (..),
    annotatorSlice,
    encodeListLen,
    encodePreEncoded,
    serialize',
    serializeEncoding,
    serializeEncoding',
  )
import qualified Cardano.Chain.Common as Byron
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
    panic,
  )
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Quiet
import Shelley.Spec.Ledger.Crypto (ADDRHASH, Crypto, DSIGN)
import Shelley.Spec.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    SignedDSIGN,
    VKey (..),
  )
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.TxData
  ( TxBody,
  )

-- | This represents the data prepended/appended to a Byron verification key
-- when producing a key hash.
data KeyPadding = KeyPadding
  { paddingPrefix :: !ByteString,
    paddingSuffix :: !ByteString
  }
  deriving (Eq, Show, Generic)

instance NoUnexpectedThunks KeyPadding

newtype ChainCode = ChainCode {unChainCode :: ByteString}
  deriving (Eq, Generic)
  deriving (Show) via Quiet ChainCode
  deriving newtype (NoUnexpectedThunks, ToCBOR, FromCBOR)

data BootstrapWitness crypto = BootstrapWitness'
  { bwKey' :: !(VKey 'Witness crypto),
    bwSig' :: !(SignedDSIGN crypto (Hash crypto (TxBody crypto))),
    bwChainCode' :: !ChainCode,
    bwPadding' :: !KeyPadding,
    bwBytes :: LByteString
  }
  deriving (Eq, Generic, Show)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn '["bwBytes"] (BootstrapWitness crypto)

pattern BootstrapWitness ::
  Crypto crypto =>
  (VKey 'Witness crypto) ->
  (SignedDSIGN crypto (Hash crypto (TxBody crypto))) ->
  ChainCode ->
  KeyPadding ->
  BootstrapWitness crypto
pattern BootstrapWitness {bwKey, bwSig, bwChainCode, bwPadding} <-
  BootstrapWitness' bwKey bwSig bwChainCode bwPadding _
  where
    BootstrapWitness key sig cc pad@(KeyPadding prefix suffix) =
      let bytes =
            serializeEncoding $
              encodeListLen 5
                <> toCBOR key
                <> DSIGN.encodeSignedDSIGN sig
                <> toCBOR cc
                <> toCBOR prefix
                <> toCBOR suffix
       in BootstrapWitness' key sig cc pad bytes

{-# COMPLETE BootstrapWitness #-}

instance
  forall crypto.
  (Crypto crypto) =>
  Ord (BootstrapWitness crypto)
  where
  compare = comparing bootstrapWitKeyHash

instance Crypto crypto => ToCBOR (BootstrapWitness crypto) where
  toCBOR = encodePreEncoded . LBS.toStrict . bwBytes

instance Crypto crypto => FromCBOR (Annotator (BootstrapWitness crypto)) where
  fromCBOR = annotatorSlice $
    decodeRecordNamed "BootstrapWitness" (const 5) $
      do
        key <- fromCBOR
        sig <- DSIGN.decodeSignedDSIGN
        cc <- fromCBOR
        prefix <- fromCBOR
        suffix <- fromCBOR
        pure . pure $ BootstrapWitness' key sig cc (KeyPadding prefix suffix)

-- | Rebuild the addrRoot of the corresponding address.
bootstrapWitKeyHash ::
  forall crypto.
  Crypto crypto =>
  BootstrapWitness crypto ->
  KeyHash 'Witness crypto
bootstrapWitKeyHash (BootstrapWitness (VKey key) _ (ChainCode cc) (KeyPadding prefix suffix)) =
  KeyHash . Hash.UnsafeHash . hash_crypto . hash_SHA3_256 $ bytes
  where
    -- Here we are reserializing something that we have previously deserialized.
    -- This is normally naughty. However, this is a blob of bytes -- serializing it
    -- amounts to wrapping the underlying byte array in a ByteString constructor.
    keyBytes = DSIGN.rawSerialiseVerKeyDSIGN key
    bytes = prefix <> keyBytes <> cc <> suffix
    hash_SHA3_256 :: ByteString -> ByteString
    hash_SHA3_256 = Hash.digest (Proxy :: Proxy Hash.SHA3_256)
    hash_crypto :: ByteString -> ByteString
    hash_crypto = Hash.digest (Proxy :: Proxy (ADDRHASH crypto))

-- | This calculates the key padding of a Byron address by serializing the
-- relevant parts.
--
-- This only supports VKey addresses, not Redeem adresses. You can also use
-- 'byronVerKeyAddressPadding' for the specific case of VKey addresses.
byronAddressPadding :: Byron.Address -> Maybe KeyPadding
byronAddressPadding (Byron.Address _ _ Byron.ATRedeem) = Nothing
byronAddressPadding (Byron.Address _ attributes Byron.ATVerKey) =
  Just (byronVerKeyAddressPadding attributes)

-- | This calculates the key padding of a Byron VKey address based only on the
-- relevant part, which is the address attributes.
byronVerKeyAddressPadding :: Byron.Attributes Byron.AddrAttributes -> KeyPadding
byronVerKeyAddressPadding attributes =
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
  KeyPadding
    { paddingPrefix =
        serializeEncoding' (encodeListLen 3) -- the surrounding 3-tuple
          <> serialize' Byron.ATVerKey
          <> serializeEncoding' (encodeListLen 2) -- the wrapper for the key
          <> serialize' (0 :: Word8) -- union tag for VKey address
          <> "\88\64", -- indicates what follows is a bytestring of length 64
      paddingSuffix = serialize' attributes
    }

unpackByronVKey ::
  forall crypto.
  (DSIGN crypto ~ Ed25519DSIGN) =>
  Byron.VerificationKey ->
  (VKey 'Witness crypto, ChainCode)
unpackByronVKey
  ( Byron.VerificationKey
      (WC.XPub vkeyBytes (WC.ChainCode chainCodeBytes))
    ) = case DSIGN.rawDeserialiseVerKeyDSIGN vkeyBytes of
    -- This maybe is produced by a check that the length of the public key
    -- is the correct one. (32 bytes). If the XPub was constructed correctly,
    -- we already know that it has this length.
    Nothing -> panic "unpackByronVKey: impossible!"
    Just vk -> (VKey vk, ChainCode chainCodeBytes)

makeBootstrapWitness ::
  forall crypto.
  ( DSIGN crypto ~ Ed25519DSIGN,
    Crypto crypto
  ) =>
  Hash crypto (TxBody crypto) ->
  Byron.SigningKey ->
  Byron.Attributes Byron.AddrAttributes ->
  BootstrapWitness crypto
makeBootstrapWitness txBodyHash byronSigningKey byronAddress =
  BootstrapWitness vk signature cc padding
  where
    (vk, cc) = unpackByronVKey $ Byron.toVerification byronSigningKey
    padding = byronVerKeyAddressPadding byronAddress
    signatureBytes =
      WC.unXSignature $
        WC.sign (mempty :: ByteString) (Byron.unSigningKey byronSigningKey) (Hash.getHash txBodyHash)
    -- This crashes in the case that the number of bytes produced when signing is
    -- different from the number of bytes expected for SigDSIGN Ed25519.
    -- At the time of writing, both of these are 64 bytes.
    signature =
      DSIGN.SignedDSIGN . fromMaybe (panic "makeBootstrapWitness: impossible!") $
        DSIGN.rawDeserialiseSigDSIGN signatureBytes
