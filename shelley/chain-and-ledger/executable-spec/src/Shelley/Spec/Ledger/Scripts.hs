{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shelley.Spec.Ledger.Scripts
  ( MultiSig
      ( RequireAllOf,
        RequireAnyOf,
        RequireSignature,
        RequireMOf,
        multiSigBytes
      ),
    Script (..),
    ScriptHash (..),
    countMSigNodes,
    getKeyCombination,
    getKeyCombinations,
    hashAnyScript,
    hashMultiSigScript,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    annotatorSlice,
    encodeListLen,
    encodePreEncoded,
    encodeWord,
    serialize',
    serializeEncoding,
  )
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Prelude
  ( AllowThunksIn (..),
    Generic,
    LByteString,
    NFData,
    NoUnexpectedThunks (..),
  )
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List (concat, concatMap, permutations)
import Data.Word (Word8)
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Hashing (HashAnnotated (..))
import Shelley.Spec.Ledger.Keys (Hash, KeyHash (..), KeyRole (Witness))
import Shelley.Spec.Ledger.Serialization (decodeList, decodeRecordSum, encodeFoldable)

-- | Magic number representing the tag of the native multi-signature script
-- language. For each script language included, a new tag is chosen and the tag
-- is included in the script hash for a script.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
data MultiSig' crypto
  = -- | Require the redeeming transaction be witnessed by the spending key
    --   corresponding to the given verification key hash.
    RequireSignature' !(KeyHash 'Witness crypto)
  | -- | Require all the sub-terms to be satisfied.
    RequireAllOf' ![MultiSig crypto]
  | -- | Require any one of the sub-terms to be satisfied.
    RequireAnyOf' ![MultiSig crypto]
  | -- | Require M of the given sub-terms to be satisfied.
    RequireMOf' !Int ![MultiSig crypto]
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NoUnexpectedThunks)

data MultiSig crypto = MultiSig'
  { multiSig :: !(MultiSig' crypto),
    multiSigBytes :: LByteString
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (NoUnexpectedThunks) via AllowThunksIn '["multiSigBytes"] (MultiSig crypto)

pattern RequireSignature :: Crypto crypto => KeyHash 'Witness crypto -> MultiSig crypto
pattern RequireSignature akh <-
  MultiSig' (RequireSignature' akh) _
  where
    RequireSignature akh =
      let bytes = serializeEncoding $ encodeListLen 2 <> encodeWord 0 <> toCBOR akh
       in MultiSig' (RequireSignature' akh) bytes

pattern RequireAllOf :: Crypto crypto => [MultiSig crypto] -> MultiSig crypto
pattern RequireAllOf ms <-
  MultiSig' (RequireAllOf' ms) _
  where
    RequireAllOf ms =
      let bytes = serializeEncoding $ encodeListLen 2 <> encodeWord 1 <> encodeFoldable ms
       in MultiSig' (RequireAllOf' ms) bytes

pattern RequireAnyOf :: Crypto crypto => [MultiSig crypto] -> MultiSig crypto
pattern RequireAnyOf ms <-
  MultiSig' (RequireAnyOf' ms) _
  where
    RequireAnyOf ms =
      let bytes = serializeEncoding $ encodeListLen 2 <> encodeWord 2 <> encodeFoldable ms
       in MultiSig' (RequireAnyOf' ms) bytes

pattern RequireMOf :: Crypto crypto => Int -> [MultiSig crypto] -> MultiSig crypto
pattern RequireMOf n ms <-
  MultiSig' (RequireMOf' n ms) _
  where
    RequireMOf n ms =
      let bytes =
            serializeEncoding $
              encodeListLen 3 <> encodeWord 3 <> toCBOR n <> encodeFoldable ms
       in MultiSig' (RequireMOf' n ms) bytes

{-# COMPLETE RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf #-}

newtype ScriptHash crypto
  = ScriptHash (Hash crypto (Script crypto))
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoUnexpectedThunks)

deriving newtype instance Crypto crypto => ToCBOR (ScriptHash crypto)

deriving newtype instance Crypto crypto => FromCBOR (ScriptHash crypto)

deriving newtype instance Crypto crypto => ToJSON (ScriptHash crypto)

deriving newtype instance Crypto crypto => FromJSON (ScriptHash crypto)

data Script crypto = MultiSigScript (MultiSig crypto)
  -- new languages go here
  deriving (Show, Eq, Ord, Generic)

instance Crypto crypto => NoUnexpectedThunks (Script crypto)

-- | Count nodes and leaves of multi signature script
countMSigNodes :: Crypto crypto => MultiSig crypto -> Int
countMSigNodes (RequireSignature _) = 1
countMSigNodes (RequireAllOf msigs) = 1 + sum (map countMSigNodes msigs)
countMSigNodes (RequireAnyOf msigs) = 1 + sum (map countMSigNodes msigs)
countMSigNodes (RequireMOf _ msigs) = 1 + sum (map countMSigNodes msigs)

-- | Hashes native multi-signature script.
hashMultiSigScript ::
  Crypto crypto =>
  MultiSig crypto ->
  ScriptHash crypto
hashMultiSigScript msig = ScriptHash $ Hash.castHash $ hashAnnotated msig

hashAnyScript ::
  Crypto crypto =>
  Script crypto ->
  ScriptHash crypto
hashAnyScript (MultiSigScript msig) = hashMultiSigScript msig

-- | Get one possible combination of keys for multi signature script
getKeyCombination :: Crypto crypto => MultiSig crypto -> [KeyHash 'Witness crypto]
getKeyCombination (RequireSignature hk) = [hk]
getKeyCombination (RequireAllOf msigs) =
  List.concatMap getKeyCombination msigs
getKeyCombination (RequireAnyOf msigs) =
  case msigs of
    [] -> []
    x : _ -> getKeyCombination x
getKeyCombination (RequireMOf m msigs) =
  List.concatMap getKeyCombination (take m msigs)

-- | Get all valid combinations of keys for given multi signature. This is
-- mainly useful for testing.
getKeyCombinations :: Crypto crypto => MultiSig crypto -> [[KeyHash 'Witness crypto]]
getKeyCombinations (RequireSignature hk) = [[hk]]
getKeyCombinations (RequireAllOf msigs) =
  [ List.concat $
      List.concatMap getKeyCombinations msigs
  ]
getKeyCombinations (RequireAnyOf msigs) = List.concatMap getKeyCombinations msigs
getKeyCombinations (RequireMOf m msigs) =
  let perms = map (take m) $ List.permutations msigs
   in map (concat . List.concatMap getKeyCombinations) perms

instance Crypto c => HashAnnotated (MultiSig c) c where
  hashAnnotated = Hash.hashWith (\x -> nativeMultiSigTag <> serialize' x)

-- CBOR

instance
  (Crypto crypto) =>
  ToCBOR (Script crypto)
  where
  toCBOR (MultiSigScript s) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR s

instance
  Crypto crypto =>
  FromCBOR (Annotator (Script crypto))
  where
  fromCBOR = decodeRecordSum "Script" $
    \case
      0 -> do
        s <- fromCBOR
        pure (2, MultiSigScript <$> s)
      k -> invalidKey k

instance
  (Crypto crypto) =>
  ToCBOR (MultiSig crypto)
  where
  toCBOR (MultiSig' _ bytes) = encodePreEncoded $ BSL.toStrict bytes

instance
  Crypto crypto =>
  FromCBOR (Annotator (MultiSig crypto))
  where
  fromCBOR = annotatorSlice $ fmap MultiSig' <$> fromCBOR

instance
  Crypto crypto =>
  FromCBOR (Annotator (MultiSig' crypto))
  where
  fromCBOR = decodeRecordSum "MultiSig" $
    \case
      0 -> (,) 2 . pure . RequireSignature' . KeyHash <$> fromCBOR
      1 -> do
        multiSigs <- sequence <$> decodeList fromCBOR
        pure (2, RequireAllOf' <$> multiSigs)
      2 -> do
        multiSigs <- sequence <$> decodeList fromCBOR
        pure (2, RequireAnyOf' <$> multiSigs)
      3 -> do
        m <- fromCBOR
        multiSigs <- sequence <$> decodeList fromCBOR
        pure $ (3, RequireMOf' m <$> multiSigs)
      k -> invalidKey k
