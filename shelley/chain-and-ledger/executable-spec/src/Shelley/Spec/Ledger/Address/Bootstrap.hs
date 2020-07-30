{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Address.Bootstrap
  ( BootstrapKeyHash,
    BootstrapVerKey (..),
    BootstrapWitness
      ( BootstrapWitness,
        bwKey,
        bwSig,
        bwChainCode,
        bwAttributes
      ),
    ChainCode (..),
    bootstrapWitKeyHash,
    unpackByronVKey,
    makeBootstrapWitness,
    verifyBootstrapWit,
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
  )
import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import qualified Cardano.Crypto.DSIGN as DSIGN
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
    panic,
  )
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Quiet
import Shelley.Spec.Ledger.Address (BootstrapKeyHash, BootstrapVerKey (..))
import Shelley.Spec.Ledger.Crypto (ADDRHASH, Crypto)
import Shelley.Spec.Ledger.Keys (Hash)
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.TxData
  ( TxBody,
  )

newtype ChainCode = ChainCode {unChainCode :: ByteString}
  deriving (Eq, Generic)
  deriving (Show) via Quiet ChainCode
  deriving newtype (NoUnexpectedThunks, ToCBOR, FromCBOR)

data BootstrapWitness crypto = BootstrapWitness'
  { bwKey' :: !BootstrapVerKey,
    bwSig' :: !(DSIGN.SignedDSIGN Ed25519DSIGN (Hash crypto (TxBody crypto))),
    bwChainCode' :: !ChainCode,
    bwAttributes' :: !ByteString,
    bwBytes :: LByteString
  }
  deriving (Eq, Generic, Show)
  deriving
    (NoUnexpectedThunks)
    via AllowThunksIn '["bwBytes"] (BootstrapWitness crypto)

pattern BootstrapWitness ::
  Crypto crypto =>
  BootstrapVerKey ->
  (DSIGN.SignedDSIGN Ed25519DSIGN (Hash crypto (TxBody crypto))) ->
  ChainCode ->
  ByteString ->
  BootstrapWitness crypto
pattern BootstrapWitness {bwKey, bwSig, bwChainCode, bwAttributes} <-
  BootstrapWitness' bwKey bwSig bwChainCode bwAttributes _
  where
    BootstrapWitness key sig cc attributes =
      let bytes =
            serializeEncoding $
              encodeListLen 4
                <> toCBOR key
                <> DSIGN.encodeSignedDSIGN sig
                <> toCBOR cc
                <> toCBOR attributes
       in BootstrapWitness' key sig cc attributes bytes

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
    decodeRecordNamed "BootstrapWitness" (const 4) $
      do
        key <- fromCBOR
        sig <- DSIGN.decodeSignedDSIGN
        cc <- fromCBOR
        attributes <- fromCBOR
        pure . pure $ BootstrapWitness' key sig cc attributes

-- | Rebuild the addrRoot of the corresponding address.
bootstrapWitKeyHash ::
  forall crypto.
  Crypto crypto =>
  BootstrapWitness crypto ->
  BootstrapKeyHash crypto
bootstrapWitKeyHash (BootstrapWitness key _ (ChainCode cc) attributes) =
  hash_crypto . hash_SHA3_256 $ bytes
  where
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
    -- the prefix is constant, and hard coded here:
    prefix :: ByteString
    prefix = "\131\00\130\00\88\64"
    -- Here we are reserializing a key which we have previously deserialized.
    -- This is normally naughty. However, this is a blob of bytes -- serializing it
    -- amounts to wrapping the underlying byte array in a ByteString constructor.
    keyBytes = DSIGN.rawSerialiseVerKeyDSIGN (unBootstrapVerKey key)
    bytes = prefix <> keyBytes <> cc <> attributes
    hash_SHA3_256 :: ByteString -> ByteString
    hash_SHA3_256 = Hash.digest (Proxy :: Proxy Hash.SHA3_256)
    hash_crypto :: ByteString -> Hash.Hash (ADDRHASH crypto) a
    hash_crypto = Hash.castHash . Hash.hashWith @(ADDRHASH crypto) id

unpackByronVKey ::
  Byron.VerificationKey ->
  (BootstrapVerKey, ChainCode)
unpackByronVKey
  ( Byron.VerificationKey
      (WC.XPub vkeyBytes (WC.ChainCode chainCodeBytes))
    ) = case DSIGN.rawDeserialiseVerKeyDSIGN vkeyBytes of
    -- This maybe is produced by a check that the length of the public key
    -- is the correct one. (32 bytes). If the XPub was constructed correctly,
    -- we already know that it has this length.
    Nothing -> panic "unpackByronVKey: impossible!"
    Just vk -> (BootstrapVerKey vk, ChainCode chainCodeBytes)

verifyBootstrapWit ::
  forall crypto.
  (Crypto crypto, DSIGN.Signable DSIGN.Ed25519DSIGN (Hash crypto (TxBody crypto))) =>
  Hash crypto (TxBody crypto) ->
  BootstrapWitness crypto ->
  Bool
verifyBootstrapWit txbodyHash witness =
  either (const False) (const True) $
    DSIGN.verifySignedDSIGN
      ()
      (unBootstrapVerKey (bwKey witness))
      txbodyHash
      (bwSig witness)

coerceSignature :: WC.XSignature -> DSIGN.SigDSIGN DSIGN.Ed25519DSIGN
coerceSignature sig =
  fromMaybe (panic "coerceSignature: impossible! signature size mismatch") $
    DSIGN.rawDeserialiseSigDSIGN (WC.unXSignature sig)

makeBootstrapWitness ::
  forall crypto.
  Crypto crypto =>
  Hash crypto (TxBody crypto) ->
  Byron.SigningKey ->
  Byron.Attributes Byron.AddrAttributes ->
  BootstrapWitness crypto
makeBootstrapWitness txBodyHash byronSigningKey addrAttributes =
  BootstrapWitness vk signature cc (serialize' addrAttributes)
  where
    (vk, cc) = unpackByronVKey $ Byron.toVerification byronSigningKey
    signature =
      DSIGN.SignedDSIGN . coerceSignature $
        WC.sign
          (mempty :: ByteString)
          (Byron.unSigningKey byronSigningKey)
          (Hash.hashToBytes txBodyHash)
