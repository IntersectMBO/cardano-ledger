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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MemoBytes is an abstraction for a data type that encodes its own serialization.
--   The idea is to use a newtype around a MemoBytes applied to a non-memoizing type.
--   For example:   newtype Foo = Foo (`MemoBytes` NonMemoizingFoo)
--   This way all the instances for @Foo (`Eq`, `Show`, `ToCBOR`, `FromCBOR`, `NoThunks`, Generic`)@
--   can be derived for free. MemoBytes plays an important role in the 'SafeToHash' class
--   introduced in the module 'Cardano.Ledger.SafeHash'
module Cardano.Ledger.MemoBytes (
  MemoBytes (Memo),
  MemoHashIndex,
  Mem,
  mkMemoBytes,
  getMemoBytesType,
  getMemoBytesHash,
  memoBytes,
  shorten,
  showMemo,
  printMemo,
  shortToLazy,
  contentsEq,

  -- * Memoized
  Memoized (RawType),
  mkMemoized,
  getMemoSafeHash,
  getMemoRawType,
  zipMemoRawType,
  getMemoRawBytes,
  lensMemoRawType,
  getterMemoRawType,
)
where

import Cardano.Crypto.Hash (HashAlgorithm (hashAlgorithmName))
import Cardano.Ledger.Binary (
  Annotator (..),
  FromCBOR (fromCBOR),
  ToCBOR,
  serialize,
  serializeEncoding,
  withSlice,
 )
import Cardano.Ledger.Binary.Coders (Encode, encode, runE)
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Core (Era (EraCrypto), eraProtVerLow)
import Cardano.Ledger.Crypto (HASH)
import Cardano.Ledger.SafeHash (SafeHash, SafeToHash (..))
import Control.DeepSeq (NFData (..))
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Coerce
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
--   from the serialization of a type, and ToCBOR instances do not have unique
--   serializations.
data MemoBytes t era = Memo'
  { mbRawType :: !(t era)
  , mbBytes :: ShortByteString
  , mbHash :: SafeHash (EraCrypto era) (MemoHashIndex t)
  }
  deriving (NoThunks) via AllowThunksIn '["mbBytes"] (MemoBytes t era)

pattern Memo :: Era era => t era -> ShortByteString -> MemoBytes t era
pattern Memo memoType memoBytes <-
  Memo' memoType memoBytes _
  where
    Memo mt mb = mkMemoBytes mt (shortToLazy mb)

{-# COMPLETE Memo #-}

type family MemoHashIndex (t :: Type -> Type) :: Type

deriving instance NFData (t era) => NFData (MemoBytes t era)

deriving instance Generic (MemoBytes t era)

instance (Typeable t, Typeable era) => Plain.EncCBOR (MemoBytes t era) where
  encCBOR (Memo' _ bytes _hash) = Plain.encodePreEncoded (fromShort bytes)

instance
  ( Typeable t
  , FromCBOR (Annotator (t era))
  , Era era
  ) =>
  FromCBOR (Annotator (MemoBytes t era))
  where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> mkMemoBytes (getT fullbytes) (getBytes fullbytes)))

-- | Both binary representation and Haskell types are compared.
instance Eq (t era) => Eq (MemoBytes t era) where
  x == y = mbBytes x == mbBytes y && mbRawType x == mbRawType y

instance (Show (t era), HashAlgorithm (HASH (EraCrypto era))) => Show (MemoBytes t era) where
  show (Memo' y _ h) =
    show y
      <> " ("
      <> hashAlgorithmName (Proxy :: Proxy (HASH (EraCrypto era)))
      <> ": "
      <> show h
      <> ")"

instance SafeToHash (MemoBytes t era) where
  originalBytes = fromShort . mbBytes

-- | Turn a lazy bytestring into a short bytestring.
shorten :: Lazy.ByteString -> ShortByteString
shorten x = toShort (toStrict x)

-- | Useful when deriving FromCBOR(Annotator T)
-- deriving via (Mem T) instance (Era era) => FromCBOR (Annotator T)
type Mem t era = Annotator (MemoBytes t era)

-- | Smart constructor
mkMemoBytes :: forall era t. Era era => t era -> BSL.ByteString -> MemoBytes t era
mkMemoBytes t bsl =
  Memo' t (toShort bs) (makeHashWithExplicitProxys (Proxy @(EraCrypto era)) (Proxy @(MemoHashIndex t)) bs)
  where
    bs = toStrict bsl

-- | Turn a MemoBytes into a string, Showing both its internal structure and its original bytes.
--   Useful since the Show instance of MemoBytes does not display the original bytes.
showMemo :: Show (t era) => MemoBytes t era -> String
showMemo (Memo' t b _) = "(Memo " ++ show t ++ "  " ++ show b ++ ")"

printMemo :: Show (t era) => MemoBytes t era -> IO ()
printMemo x = putStrLn (showMemo x)

-- | Create MemoBytes from its CBOR encoding
memoBytes :: forall era w t. Era era => Encode w (t era) -> MemoBytes t era
memoBytes t = mkMemoBytes (runE t) (serializeEncoding (eraProtVerLow @era) (encode t))

-- | Helper function. Converts a short bytestring to a lazy bytestring.
shortToLazy :: ShortByteString -> BSL.ByteString
shortToLazy = fromStrict . fromShort

-- | Returns true if the contents of the MemoBytes are equal
contentsEq :: Eq (t era) => MemoBytes t era -> MemoBytes t era -> Bool
contentsEq x y = mbRawType x == mbRawType y

-- | Extract the inner type of the MemoBytes
getMemoBytesType :: MemoBytes t era -> t era
getMemoBytesType = mbRawType

-- | Extract the hash value of the binary representation of the MemoBytes
getMemoBytesHash :: MemoBytes t era -> SafeHash (EraCrypto era) (MemoHashIndex t)
getMemoBytesHash = mbHash

-- | Class that relates the actual type with its raw and byte representations
class Memoized t where
  type RawType t = (r :: Type -> Type) | r -> t

  -- | This is a coercion from the memoized type to the MemoBytes. This implementation
  -- cannot be changed since `getMemoBytes` is not exported, therefore it will only work
  -- on newtypes around `MemoBytes`
  getMemoBytes :: t era -> MemoBytes (RawType t) era
  default getMemoBytes ::
    Coercible (t era) (MemoBytes (RawType t) era) =>
    t era ->
    MemoBytes (RawType t) era
  getMemoBytes = coerce

  -- | This is a coercion from the MemoBytes to the momoized type. This implementation
  -- cannot be changed since `warpMemoBytes` is not exported, therefore it will only work
  -- on newtypes around `MemoBytes`
  wrapMemoBytes :: MemoBytes (RawType t) era -> t era
  default wrapMemoBytes ::
    Coercible (MemoBytes (RawType t) era) (t era) =>
    MemoBytes (RawType t) era ->
    t era
  wrapMemoBytes = coerce

-- | Construct memoized type from the raw type using its ToCBOR instance
mkMemoized :: forall era t. (Era era, ToCBOR (RawType t era), Memoized t) => RawType t era -> t era
mkMemoized rawType = wrapMemoBytes (mkMemoBytes rawType (serialize (eraProtVerLow @era) rawType))

-- | Extract memoized SafeHash
getMemoSafeHash :: Memoized t => t era -> SafeHash (EraCrypto era) (MemoHashIndex (RawType t))
getMemoSafeHash t = mbHash (getMemoBytes t)

-- | Extract the raw type from the memoized version
getMemoRawType :: Memoized t => t era -> RawType t era
getMemoRawType t = mbRawType (getMemoBytes t)

-- | Extract the raw bytes from the memoized version
getMemoRawBytes :: Memoized t => t era -> ShortByteString
getMemoRawBytes t = mbBytes (getMemoBytes t)

-- | This is a helper function that operates on raw types of two memoized types.
zipMemoRawType ::
  (Memoized t1, Memoized t2) =>
  (RawType t1 era -> RawType t2 era -> a) ->
  t1 era ->
  t2 era ->
  a
zipMemoRawType f x y = f (getMemoRawType x) (getMemoRawType y)

-- | This is a helper Lens creator for any Memoized type.
lensMemoRawType ::
  (Era era, ToCBOR (RawType t era), Memoized t) =>
  (RawType t era -> a) ->
  (RawType t era -> b -> RawType t era) ->
  Lens (t era) (t era) a b
lensMemoRawType getter setter =
  lens (getter . getMemoRawType) (\t v -> mkMemoized $ setter (getMemoRawType t) v)
{-# INLINEABLE lensMemoRawType #-}

-- | This is a helper SimpleGetter creator for any Memoized type
getterMemoRawType ::
  Memoized t =>
  (RawType t era -> a) ->
  SimpleGetter (t era) a
getterMemoRawType getter =
  to (getter . getMemoRawType)
{-# INLINEABLE getterMemoRawType #-}
