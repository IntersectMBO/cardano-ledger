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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Keys.Bootstrap (
  BootstrapWitness (
    BootstrapWitness,
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
  eqBootstrapWitnessRaw,
)
where

import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.DSIGN (SignedDSIGN (..))
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as WC
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  EncCBOR (..),
  annotatorSlice,
  byronProtVer,
  decodeRecordNamed,
  encodeListLen,
  serialize,
  serialize',
 )
import Cardano.Ledger.Binary.Crypto (
  decodeSignedDSIGN,
  encodeSignedDSIGN,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Hashes (ADDRHASH, EraIndependentTxBody, HASH, Hash, KeyHash (..))
import Cardano.Ledger.Keys.Internal (
  DSIGN,
  KeyRole (..),
  VKey (..),
  verifySignedDSIGN,
 )
import Cardano.Ledger.MemoBytes (
  EqRaw (..),
  MemoBytes (Memo),
  decodeMemoized,
 )
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
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
  deriving newtype (NoThunks, EncCBOR, DecCBOR, NFData)

data BootstrapWitness = BootstrapWitness'
  { bwKey' :: !(VKey 'Witness)
  , bwSig' :: !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
  , bwChainCode' :: !ChainCode
  , bwAttributes' :: !ByteString
  , bwBytes :: LBS.ByteString
  }
  deriving (Generic, Show, Eq)

instance NFData BootstrapWitness

deriving via
  (AllowThunksIn '["bwBytes"] BootstrapWitness)
  instance
    NoThunks BootstrapWitness

pattern BootstrapWitness ::
  VKey 'Witness ->
  SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody) ->
  ChainCode ->
  ByteString ->
  BootstrapWitness
pattern BootstrapWitness {bwKey, bwSig, bwChainCode, bwAttributes} <-
  BootstrapWitness' bwKey bwSig bwChainCode bwAttributes _
  where
    BootstrapWitness key sig cc attributes =
      let bytes =
            serialize byronProtVer $
              encodeListLen 4
                <> encCBOR key
                <> encodeSignedDSIGN sig
                <> encCBOR cc
                <> encCBOR attributes
       in BootstrapWitness' key sig cc attributes bytes

{-# COMPLETE BootstrapWitness #-}

instance Ord BootstrapWitness where
  compare = comparing bootstrapWitKeyHash

instance Plain.ToCBOR BootstrapWitness where
  toCBOR = Plain.encodePreEncoded . LBS.toStrict . bwBytes

-- | Encodes memoized bytes created upon construction.
instance EncCBOR BootstrapWitness

instance DecCBOR (Annotator BootstrapWitness) where
  decCBOR = annotatorSlice $
    decodeRecordNamed "BootstrapWitness" (const 4) $ do
      key <- decCBOR
      sig <- decodeSignedDSIGN
      cc <- decCBOR
      attributes <- decCBOR
      pure . pure $ BootstrapWitness' key sig cc attributes

data BootstrapWitnessRaw
  = BootstrapWitnessRaw
      !(VKey 'Witness)
      !(SignedDSIGN DSIGN (Hash HASH EraIndependentTxBody))
      !ChainCode
      !ByteString

instance DecCBOR BootstrapWitnessRaw where
  decCBOR =
    decodeRecordNamed "BootstrapWitnessRaw" (const 4) $
      BootstrapWitnessRaw <$> decCBOR <*> decodeSignedDSIGN <*> decCBOR <*> decCBOR

instance DecCBOR BootstrapWitness where
  decCBOR = do
    Memo (BootstrapWitnessRaw k s c a) bs <- decodeMemoized (decCBOR @BootstrapWitnessRaw)
    pure $ BootstrapWitness' k s c a (LBS.fromStrict (SBS.fromShort bs))

-- | Rebuild the addrRoot of the corresponding address.
bootstrapWitKeyHash ::
  BootstrapWitness ->
  KeyHash 'Witness
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
    hash_crypto :: ByteString -> Hash.Hash ADDRHASH a
    hash_crypto = Hash.castHash . Hash.hashWith @ADDRHASH id

unpackByronVKey ::
  Byron.VerificationKey ->
  (VKey 'Witness, ChainCode)
unpackByronVKey
  ( Byron.VerificationKey
      (WC.XPub vkeyBytes (WC.ChainCode chainCodeBytes))
    ) = case DSIGN.rawDeserialiseVerKeyDSIGN vkeyBytes of
    -- This maybe is produced by a check that the length of the public key
    -- is the correct one. (32 bytes). If the XPub was constructed correctly,
    -- we already know that it has this length.
    Nothing -> error "unpackByronVKey: impossible!"
    Just vk -> (VKey vk, ChainCode chainCodeBytes)

verifyBootstrapWit ::
  Hash HASH EraIndependentTxBody ->
  BootstrapWitness ->
  Bool
verifyBootstrapWit txbodyHash witness =
  verifySignedDSIGN
    (bwKey witness)
    txbodyHash
    (coerce . bwSig $ witness)

coerceSignature :: WC.XSignature -> DSIGN.SigDSIGN DSIGN.Ed25519DSIGN
coerceSignature sig =
  fromMaybe (error "coerceSignature: impossible! signature size mismatch") $
    DSIGN.rawDeserialiseSigDSIGN (WC.unXSignature sig)

makeBootstrapWitness ::
  Hash HASH EraIndependentTxBody ->
  Byron.SigningKey ->
  Byron.Attributes Byron.AddrAttributes ->
  BootstrapWitness
makeBootstrapWitness txBodyHash byronSigningKey addrAttributes =
  BootstrapWitness vk signature cc (serialize' byronProtVer addrAttributes)
  where
    (vk, cc) = unpackByronVKey $ Byron.toVerification byronSigningKey
    signature =
      SignedDSIGN . coerceSignature $
        WC.sign
          (mempty :: ByteString)
          (Byron.unSigningKey byronSigningKey)
          (Hash.hashToBytes txBodyHash)

eqBootstrapWitnessRaw :: BootstrapWitness -> BootstrapWitness -> Bool
eqBootstrapWitnessRaw bw1 bw2 =
  bwKey bw1 == bwKey bw2
    && bwSig bw1 == bwSig bw2
    && bwChainCode bw1 == bwChainCode bw2
    && bwAttributes bw1 == bwAttributes bw2

instance EqRaw BootstrapWitness where
  eqRaw = eqBootstrapWitnessRaw
