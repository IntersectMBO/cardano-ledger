{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides MemoBytes internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.MemoBytes.Internal (
  MemoBytes (.., Memo),
  Mem,
  MemoHashIndex,
  mkMemoBytes,
  mkMemoBytesStrict,
  getMemoBytesType,
  getMemoBytesHash,
  memoBytes,
  memoBytesEra,
  shorten,
  showMemo,
  printMemo,
  shortToLazy,
  contentsEq,
  decodeMemoBytes,

  -- * Memoized
  Memoized (RawType),
  mkMemoized,
  mkMemoizedEra,
  decodeMemoized,
  getMemoSafeHash,
  getMemoRawType,
  zipMemoRawType,
  eqRawType,
  getMemoRawBytes,
  lensMemoRawType,
  getterMemoRawType,

  -- * MemoBytes MemPack instance definitions
  byteCountMemoBytes,
  packMemoBytesM,
  unpackMemoBytesM,

  -- * Raw equality
  EqRaw (..),
) where

import Cardano.Crypto.Hash (HashAlgorithm (hashAlgorithmName))
import Cardano.Ledger.Binary (
  Annotated (..),
  Annotator (..),
  DecCBOR (decCBOR),
  Decoder,
  EncCBOR,
  Version,
  decodeAnnotated,
  decodeFullAnnotator,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Binary.Coders (Encode, encode, runE)
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core.Era (Era, eraProtVerLow)
import Cardano.Ledger.Hashes (HASH, SafeHash, SafeToHash (..))
import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import qualified Data.ByteString.Short as SBS (length)
import Data.Coerce
import Data.MemPack
import Data.MemPack.Buffer (Buffer)
import qualified Data.Text as T
import Data.Typeable
import GHC.Base (Type)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Prelude hiding (span)

-- ========================================================================

-- | Pair together a type @t@ and its serialization. Used to encode a type
--   that is serialized over the network, and to remember the original bytes
--   that were used to transmit it. Important since hashes are computed
--   from the serialization of a type, and EncCBOR instances do not have unique
--   serializations.
data MemoBytes t = MemoBytes
  { mbRawType :: !t
  , mbBytes :: ShortByteString
  , mbHash :: SafeHash (MemoHashIndex t)
  }
  deriving (Generic)
  deriving (NoThunks) via AllowThunksIn '["mbBytes", "mbHash"] (MemoBytes t)

-- | Prevent coercion on MemoBytes because it does not preserve the invariants
type role MemoBytes nominal

pattern Memo :: t -> ShortByteString -> MemoBytes t
pattern Memo memoType memoBytes <-
  MemoBytes memoType memoBytes _
  where
    Memo mt mb = mkMemoBytes mt (shortToLazy mb)

{-# COMPLETE Memo #-}

byteCountMemoBytes :: MemoBytes t -> Int
byteCountMemoBytes = packedByteCount . mbBytes

packMemoBytesM :: MemoBytes t -> Pack s ()
packMemoBytesM = packM . mbBytes

unpackMemoBytesM ::
  ( DecCBOR (Annotator t)
  , Typeable t
  , Buffer b
  ) =>
  Version -> Unpack b (MemoBytes t)
unpackMemoBytesM v = unpackM >>= decodeMemoBytes v

decodeMemoBytes ::
  forall t m.
  ( Typeable t
  , DecCBOR (Annotator t)
  , MonadFail m
  ) =>
  Version -> ByteString -> m (MemoBytes t)
decodeMemoBytes v bs =
  either (fail . show) pure $
    decodeFullAnnotator
      v
      (T.pack (show (typeRep (Proxy @t))))
      decCBOR
      (BSL.fromStrict bs)

type family MemoHashIndex (t :: Type) :: Type

deriving instance NFData t => NFData (MemoBytes t)

instance Typeable t => Plain.ToCBOR (MemoBytes t) where
  toCBOR (MemoBytes _ bytes _hash) = Plain.encodePreEncoded (fromShort bytes)

instance DecCBOR t => DecCBOR (MemoBytes t) where
  decCBOR = decodeMemoized decCBOR

instance
  (Typeable t, DecCBOR (Annotator t)) =>
  DecCBOR (Annotator (MemoBytes t))
  where
  decCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice decCBOR
    pure $ Annotator (\fullbytes -> mkMemoBytes (getT fullbytes) (getBytes fullbytes))

-- | Both binary representation and Haskell types are compared.
instance Eq t => Eq (MemoBytes t) where
  x == y = mbBytes x == mbBytes y && mbRawType x == mbRawType y

instance Show t => Show (MemoBytes t) where
  show (MemoBytes y _ h) =
    show y
      <> " ("
      <> hashAlgorithmName (Proxy :: Proxy HASH)
      <> ": "
      <> show h
      <> ")"

instance SafeToHash (MemoBytes t) where
  originalBytes = fromShort . mbBytes
  originalBytesSize = SBS.length . mbBytes

-- | Turn a lazy bytestring into a short bytestring.
shorten :: BSL.ByteString -> ShortByteString
shorten x = toShort (toStrict x)
{-# DEPRECATED shorten "As unused. Use `toShort` `.` `toStrict` instead" #-}

-- | Useful when deriving DecCBOR(Annotator T)
-- deriving via (Mem T) instance DecCBOR (Annotator T)
type Mem t = Annotator (MemoBytes t)

-- | Constructor that takes the underlying type and the original bytes as lazy
-- `BSL.ByteString`.
--
-- /Warning/ - This is a dangerous constructor because it allows one to construct a `MemoBytes` type
-- with wrong bytes.
mkMemoBytes :: t -> BSL.ByteString -> MemoBytes t
mkMemoBytes t = mkMemoBytesStrict t . toStrict

-- | Same as `mkMemoBytes`, but with strict bytes
mkMemoBytesStrict :: forall t. t -> ByteString -> MemoBytes t
mkMemoBytesStrict t bs =
  MemoBytes t (toShort bs) $
    makeHashWithExplicitProxys (Proxy @(MemoHashIndex t)) bs

-- | Turn a MemoBytes into a string, showing both its internal structure and its original bytes.
--   Useful since the Show instance of MemoBytes does not display the original bytes.
showMemo :: Show t => MemoBytes t -> String
showMemo (MemoBytes t b _) = "(Memo " ++ show t ++ "  " ++ show b ++ ")"
{-# DEPRECATED showMemo "As unused. Show instance will show the hash, which is enough most of the time" #-}

printMemo :: Show t => MemoBytes t -> IO ()
printMemo x = putStrLn (showMemo x)
{-# DEPRECATED printMemo "As unused. Show instance will show the hash, which is enough most of the time" #-}

-- | Create MemoBytes from its CBOR encoding
--
-- /Warning/ - This is a dangerous constructor because it allows one to construct a `MemoBytes` type
-- from the wrong encoding. Use `mkMemoized` instead when possible.
memoBytes :: Version -> Encode w t -> MemoBytes t
memoBytes v t = mkMemoBytes (runE t) (serialize v (encode t))

-- | Same as `memoBytes`, but derives `Version` from the era.
--
-- /Warning/ - This is a dangerous constructor because it allows one to construct a `MemoBytes` type
-- from the wrong encoding. Use `mkMemoizedEra` instead when possible.
memoBytesEra :: forall era w t. Era era => Encode w t -> MemoBytes t
memoBytesEra = memoBytes (eraProtVerLow @era)

-- | Helper function. Converts a short bytestring to a lazy bytestring.
shortToLazy :: ShortByteString -> BSL.ByteString
shortToLazy = fromStrict . fromShort
{-# INLINE shortToLazy #-}

-- | Returns true if the contents of the MemoBytes are equal
contentsEq :: Eq t => MemoBytes t -> MemoBytes t -> Bool
contentsEq x y = mbRawType x == mbRawType y

-- | Extract the inner type of the MemoBytes
getMemoBytesType :: MemoBytes t -> t
getMemoBytesType = mbRawType

-- | Extract the hash value of the binary representation of the MemoBytes
getMemoBytesHash :: MemoBytes t -> SafeHash (MemoHashIndex t)
getMemoBytesHash = mbHash

-- | Class that relates the actual type with its raw and byte representations
class Memoized t where
  type RawType t = (r :: Type) | r -> t

  -- | This is a coercion from the memoized type to the MemoBytes. This implementation
  -- cannot be changed since `getMemoBytes` is not exported, therefore it will only work
  -- on newtypes around `MemoBytes`
  getMemoBytes :: t -> MemoBytes (RawType t)
  default getMemoBytes ::
    Coercible t (MemoBytes (RawType t)) =>
    t ->
    MemoBytes (RawType t)
  getMemoBytes = coerce

  -- | This is a coercion from the MemoBytes to the memoized type. This implementation
  -- cannot be changed since `warpMemoBytes` is not exported, therefore it will only work
  -- on newtypes around `MemoBytes`
  wrapMemoBytes :: MemoBytes (RawType t) -> t
  default wrapMemoBytes ::
    Coercible (MemoBytes (RawType t)) t =>
    MemoBytes (RawType t) ->
    t
  wrapMemoBytes = coerce

-- | Construct memoized type from the raw type using its EncCBOR instance
mkMemoized :: forall t. (EncCBOR (RawType t), Memoized t) => Version -> RawType t -> t
mkMemoized v rawType = wrapMemoBytes (mkMemoBytes rawType (serialize v rawType))

mkMemoizedEra :: forall era t. (Era era, EncCBOR (RawType t), Memoized t) => RawType t -> t
mkMemoizedEra = mkMemoized (eraProtVerLow @era)

decodeMemoized :: Decoder s t -> Decoder s (MemoBytes t)
decodeMemoized rawTypeDecoder = do
  Annotated rawType lazyBytes <- decodeAnnotated rawTypeDecoder
  pure $ mkMemoBytes rawType lazyBytes

-- | Extract memoized SafeHash
getMemoSafeHash :: Memoized t => t -> SafeHash (MemoHashIndex (RawType t))
getMemoSafeHash t = mbHash (getMemoBytes t)

-- | Extract the raw type from the memoized version
getMemoRawType :: Memoized t => t -> RawType t
getMemoRawType t = mbRawType (getMemoBytes t)

-- | Extract the raw bytes from the memoized version
getMemoRawBytes :: Memoized t => t -> ShortByteString
getMemoRawBytes t = mbBytes (getMemoBytes t)

-- | This is a helper function that operates on raw types of two memoized types.
zipMemoRawType ::
  (Memoized t1, Memoized t2) =>
  (RawType t1 -> RawType t2 -> a) ->
  t1 ->
  t2 ->
  a
zipMemoRawType f x y = f (getMemoRawType x) (getMemoRawType y)

eqRawType ::
  forall t.
  (Memoized t, Eq (RawType t)) =>
  t ->
  t ->
  Bool
eqRawType = zipMemoRawType @t (==)

-- | This is a helper Lens creator for any Memoized type.
lensMemoRawType ::
  forall era t a b.
  (Era era, EncCBOR (RawType t), Memoized t) =>
  (RawType t -> a) ->
  (RawType t -> b -> RawType t) ->
  Lens t t a b
lensMemoRawType getter setter =
  lens (getter . getMemoRawType) (\t b -> mkMemoizedEra @era $ setter (getMemoRawType t) b)
{-# INLINEABLE lensMemoRawType #-}

-- | This is a helper SimpleGetter creator for any Memoized type
getterMemoRawType ::
  Memoized t =>
  (RawType t -> a) ->
  SimpleGetter t a
getterMemoRawType getter =
  to (getter . getMemoRawType)
{-# INLINEABLE getterMemoRawType #-}

-- | Type class that implements equality on the Haskell type, ignoring any of the
-- potentially memoized binary representation of the type.
class EqRaw a where
  eqRaw :: a -> a -> Bool
  default eqRaw :: (a ~ t, Memoized t, Eq (RawType t)) => a -> a -> Bool
  eqRaw = eqRawType
