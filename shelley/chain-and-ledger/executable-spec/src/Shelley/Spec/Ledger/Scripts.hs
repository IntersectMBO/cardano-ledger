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
import Cardano.Ledger.Crypto (ADDRHASH)
import Cardano.Ledger.Era (Crypto (..))
import Control.DeepSeq (NFData)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List (concat, concatMap, permutations)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (invalidKey)
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (Witness))
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
data MultiSig' era
  = -- | Require the redeeming transaction be witnessed by the spending key
    --   corresponding to the given verification key hash.
    RequireSignature' !(KeyHash 'Witness (Crypto era))
  | -- | Require all the sub-terms to be satisfied.
    RequireAllOf' ![MultiSig era]
  | -- | Require any one of the sub-terms to be satisfied.
    RequireAnyOf' ![MultiSig era]
  | -- | Require M of the given sub-terms to be satisfied.
    RequireMOf' !Int ![MultiSig era]
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NoThunks)

data MultiSig era = MultiSig'
  { multiSig :: !(MultiSig' era),
    multiSigBytes :: BSL.ByteString
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (NoThunks) via AllowThunksIn '["multiSigBytes"] (MultiSig era)

pattern RequireSignature :: Era era => KeyHash 'Witness (Crypto era) -> MultiSig era
pattern RequireSignature akh <-
  MultiSig' (RequireSignature' akh) _
  where
    RequireSignature akh =
      let bytes = serializeEncoding $ encodeListLen 2 <> encodeWord 0 <> toCBOR akh
       in MultiSig' (RequireSignature' akh) bytes

pattern RequireAllOf :: Era era => [MultiSig era] -> MultiSig era
pattern RequireAllOf ms <-
  MultiSig' (RequireAllOf' ms) _
  where
    RequireAllOf ms =
      let bytes = serializeEncoding $ encodeListLen 2 <> encodeWord 1 <> encodeFoldable ms
       in MultiSig' (RequireAllOf' ms) bytes

pattern RequireAnyOf :: Era era => [MultiSig era] -> MultiSig era
pattern RequireAnyOf ms <-
  MultiSig' (RequireAnyOf' ms) _
  where
    RequireAnyOf ms =
      let bytes = serializeEncoding $ encodeListLen 2 <> encodeWord 2 <> encodeFoldable ms
       in MultiSig' (RequireAnyOf' ms) bytes

pattern RequireMOf :: Era era => Int -> [MultiSig era] -> MultiSig era
pattern RequireMOf n ms <-
  MultiSig' (RequireMOf' n ms) _
  where
    RequireMOf n ms =
      let bytes =
            serializeEncoding $
              encodeListLen 3 <> encodeWord 3 <> toCBOR n <> encodeFoldable ms
       in MultiSig' (RequireMOf' n ms) bytes

{-# COMPLETE RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf #-}

newtype ScriptHash era
  = ScriptHash (Hash.Hash (ADDRHASH (Crypto era)) (Script era))
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance Era era => ToCBOR (ScriptHash era)

deriving newtype instance Era era => FromCBOR (ScriptHash era)

deriving newtype instance Era era => ToJSON (ScriptHash era)

deriving newtype instance Era era => FromJSON (ScriptHash era)

data Script era = MultiSigScript (MultiSig era)
  -- new languages go here
  deriving (Show, Eq, Ord, Generic)

instance Era era => NoThunks (Script era)

-- | Hashes native multi-signature script.
hashMultiSigScript ::
  Era era =>
  MultiSig era ->
  ScriptHash era
hashMultiSigScript =
  ScriptHash
    . Hash.castHash
    . Hash.hashWith (\x -> nativeMultiSigTag <> serialize' x)

hashAnyScript ::
  Era era =>
  Script era ->
  ScriptHash era
hashAnyScript (MultiSigScript msig) = hashMultiSigScript msig

-- | Get one possible combination of keys for multi signature script
getKeyCombination :: Era era => MultiSig era -> [KeyHash 'Witness (Crypto era)]
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
getKeyCombinations :: Era era => MultiSig era -> [[KeyHash 'Witness (Crypto era)]]
getKeyCombinations (RequireSignature hk) = [[hk]]
getKeyCombinations (RequireAllOf msigs) =
  [ List.concat $
      List.concatMap getKeyCombinations msigs
  ]
getKeyCombinations (RequireAnyOf msigs) = List.concatMap getKeyCombinations msigs
getKeyCombinations (RequireMOf m msigs) =
  let perms = map (take m) $ List.permutations msigs
   in map (concat . List.concatMap getKeyCombinations) perms

-- CBOR

instance
  (Era era) =>
  ToCBOR (Script era)
  where
  toCBOR (MultiSigScript s) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR s

instance
  Era era =>
  FromCBOR (Annotator (Script era))
  where
  fromCBOR = decodeRecordSum "Script" $
    \case
      0 -> do
        s <- fromCBOR
        pure (2, MultiSigScript <$> s)
      k -> invalidKey k

instance
  (Era era) =>
  ToCBOR (MultiSig era)
  where
  toCBOR (MultiSig' _ bytes) = encodePreEncoded $ BSL.toStrict bytes

instance
  Era era =>
  FromCBOR (Annotator (MultiSig era))
  where
  fromCBOR = annotatorSlice $ fmap MultiSig' <$> fromCBOR

instance
  Era era =>
  FromCBOR (Annotator (MultiSig' era))
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
