{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Address.Bootstrap
  ( BootstrapWitness
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
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as WC
import Cardano.Ledger.Crypto (ADDRHASH, DSIGN)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys
  ( Hash,
    KeyHash (..),
    KeyRole (..),
    VKey (..),
    verifySignedDSIGN,
  )
import qualified Cardano.Ledger.Keys as Keys
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Prelude (panic)
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Quiet

newtype ChainCode = ChainCode {unChainCode :: ByteString}
  deriving (Eq, Generic)
  deriving (Show) via Quiet ChainCode
  deriving newtype (NoThunks, ToCBOR, FromCBOR, NFData)

data BootstrapWitness crypto = BootstrapWitness'
  { bwKey' :: !(VKey 'Witness crypto),
    bwSig' ::
      !( Keys.SignedDSIGN
           crypto
           (Hash crypto EraIndependentTxBody)
       ),
    bwChainCode' :: !ChainCode,
    bwAttributes' :: !ByteString,
    bwBytes :: LBS.ByteString
  }
  deriving (Generic)

deriving instance CC.Crypto crypto => Show (BootstrapWitness crypto)

deriving instance CC.Crypto crypto => Eq (BootstrapWitness crypto)

instance
  ( CC.Crypto era,
    NFData (DSIGN.VerKeyDSIGN (DSIGN era)),
    NFData (DSIGN.SigDSIGN (DSIGN era))
  ) =>
  NFData (BootstrapWitness era)

deriving via
  (AllowThunksIn '["bwBytes"] (BootstrapWitness crypto))
  instance
    CC.Crypto crypto => NoThunks (BootstrapWitness crypto)

pattern BootstrapWitness ::
  CC.Crypto crypto =>
  VKey 'Witness crypto ->
  Keys.SignedDSIGN crypto (Hash crypto EraIndependentTxBody) ->
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

instance CC.Crypto crypto => Ord (BootstrapWitness crypto) where
  compare = comparing bootstrapWitKeyHash

instance CC.Crypto crypto => ToCBOR (BootstrapWitness crypto) where
  toCBOR = encodePreEncoded . LBS.toStrict . bwBytes

instance CC.Crypto crypto => FromCBOR (Annotator (BootstrapWitness crypto)) where
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
  CC.Crypto crypto =>
  BootstrapWitness crypto ->
  KeyHash 'Witness crypto
bootstrapWitKeyHash (BootstrapWitness (VKey key) _ (ChainCode cc) attributes) =
  KeyHash . hash_crypto . hash_SHA3_256 $ bytes
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
    -- This is normally naughty. However, this is a blob of bytes -- serializing
    -- it amounts to wrapping the underlying byte array in a ByteString
    -- constructor.
    keyBytes = DSIGN.rawSerialiseVerKeyDSIGN key
    bytes = prefix <> keyBytes <> cc <> attributes
    hash_SHA3_256 :: ByteString -> ByteString
    hash_SHA3_256 = Hash.digest (Proxy :: Proxy Hash.SHA3_256)
    hash_crypto :: ByteString -> Hash.Hash (ADDRHASH crypto) a
    hash_crypto = Hash.castHash . Hash.hashWith @(ADDRHASH crypto) id

unpackByronVKey ::
  forall crypto.
  (DSIGN crypto ~ DSIGN.Ed25519DSIGN) =>
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

verifyBootstrapWit ::
  forall crypto.
  ( CC.Crypto crypto,
    DSIGN.Signable (DSIGN crypto) (Hash crypto EraIndependentTxBody)
  ) =>
  Hash crypto EraIndependentTxBody ->
  BootstrapWitness crypto ->
  Bool
verifyBootstrapWit txbodyHash witness =
  verifySignedDSIGN
    (bwKey witness)
    txbodyHash
    (coerce . bwSig $ witness)

coerceSignature :: WC.XSignature -> DSIGN.SigDSIGN DSIGN.Ed25519DSIGN
coerceSignature sig =
  fromMaybe (panic "coerceSignature: impossible! signature size mismatch") $
    DSIGN.rawDeserialiseSigDSIGN (WC.unXSignature sig)

makeBootstrapWitness ::
  forall crypto.
  ( DSIGN crypto ~ DSIGN.Ed25519DSIGN,
    CC.Crypto crypto
  ) =>
  Hash crypto EraIndependentTxBody ->
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
