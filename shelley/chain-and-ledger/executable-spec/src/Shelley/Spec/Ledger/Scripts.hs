{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.Scripts
  ( MultiSig
      ( RequireAllOf,
        RequireAnyOf,
        RequireSignature,
        RequireMOf
      ),
    getMultiSigBytes,
    ScriptHash (..),
    getKeyCombination,
    getKeyCombinations,
    hashMultiSigScript,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR,
    serialize',
  )
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto (ADDRHASH)
import Cardano.Ledger.Era (Crypto (..))
import Control.DeepSeq (NFData)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Data.Coders (Encode (..), (!>))
import qualified Data.List as List (concat, concatMap, permutations)
import Data.MemoBytes
  ( Mem,
    MemoBytes (..),
    memoBytes,
  )
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
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
data MultiSigRaw era
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

newtype MultiSig era = MultiSigConstr (MemoBytes (MultiSigRaw era))
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToCBOR, NoThunks)

getMultiSigBytes :: MultiSig era -> ShortByteString
getMultiSigBytes (MultiSigConstr (Memo _ bytes)) = bytes

deriving via
  (Mem (MultiSigRaw era))
  instance
    (Era era) =>
    FromCBOR (Annotator (MultiSig era))

pattern RequireSignature :: Era era => KeyHash 'Witness (Crypto era) -> MultiSig era
pattern RequireSignature akh <-
  MultiSigConstr (Memo (RequireSignature' akh) _)
  where
    RequireSignature akh =
      MultiSigConstr $ memoBytes (Sum RequireSignature' 0 !> To akh)

pattern RequireAllOf :: Era era => [MultiSig era] -> MultiSig era
pattern RequireAllOf ms <-
  MultiSigConstr (Memo (RequireAllOf' ms) _)
  where
    RequireAllOf ms =
      MultiSigConstr $ memoBytes (Sum RequireAllOf' 1 !> E encodeFoldable ms)

pattern RequireAnyOf :: Era era => [MultiSig era] -> MultiSig era
pattern RequireAnyOf ms <-
  MultiSigConstr (Memo (RequireAnyOf' ms) _)
  where
    RequireAnyOf ms =
      MultiSigConstr $ memoBytes (Sum RequireAnyOf' 2 !> E encodeFoldable ms)

pattern RequireMOf :: Era era => Int -> [MultiSig era] -> MultiSig era
pattern RequireMOf n ms <-
  MultiSigConstr (Memo (RequireMOf' n ms) _)
  where
    RequireMOf n ms =
      MultiSigConstr $ memoBytes (Sum RequireMOf' 3 !> To n !> E encodeFoldable ms)

{-# COMPLETE RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf #-}

newtype ScriptHash era
  = ScriptHash (Hash.Hash (ADDRHASH (Crypto era)) (Core.Script era))
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, NoThunks)

deriving newtype instance
  (Era era, Typeable (Core.Script era)) =>
  ToCBOR (ScriptHash era)

deriving newtype instance
  (Era era, Typeable (Core.Script era)) =>
  FromCBOR (ScriptHash era)

deriving newtype instance (Era era) => ToJSON (ScriptHash era)

deriving newtype instance Era era => FromJSON (ScriptHash era)

-- | Hashes native multi-signature script.
hashMultiSigScript ::
  Era era =>
  MultiSig era ->
  ScriptHash era
hashMultiSigScript =
  ScriptHash
    . Hash.castHash
    . Hash.hashWith (\x -> nativeMultiSigTag <> serialize' x)

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
  Era era =>
  FromCBOR (Annotator (MultiSigRaw era))
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
