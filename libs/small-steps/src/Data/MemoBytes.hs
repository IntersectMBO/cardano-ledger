{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MemoBytes is an abstraction for a data type that encodes its own serialization.
--   The idea is to use a newtype around a MemoBytes non-memoizing version.
--   For example:   newtype Foo = Foo (MemoBytes NonMemoizingFoo)
--   This way all the instances for Foo (Eq,Show,Ord,ToCBOR,FromCBOR,NoThunks,Generic)
--   can be derived for free.
module Data.MemoBytes
  ( MemoBytes (..),
    memoBytes,
    Mem,
    shorten,
    showMemo,
    printMemo,
    roundTripMemo,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodePreEncoded,
    withSlice,
  )
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.DeepSeq (NFData (..))
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Coders (Encode, encode, runE)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Prelude hiding (span)

-- ========================================================================

data MemoBytes t = Memo {memotype :: !t, memobytes :: ShortByteString}
  deriving (NoThunks) via AllowThunksIn '["memobytes"] (MemoBytes t)

deriving instance NFData t => NFData (MemoBytes t)

deriving instance Generic (MemoBytes t)

instance (Typeable t) => ToCBOR (MemoBytes t) where
  toCBOR (Memo _ bytes) = encodePreEncoded (fromShort bytes)

instance (Typeable t, FromCBOR (Annotator t)) => FromCBOR (Annotator (MemoBytes t)) where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> Memo (getT fullbytes) (toShort (toStrict (getBytes fullbytes)))))

instance Eq (MemoBytes t) where (Memo _ x) == (Memo _ y) = x == y

instance Show t => Show (MemoBytes t) where show (Memo y _) = show y

instance Ord t => Ord (MemoBytes t) where compare (Memo x _) (Memo y _) = compare x y

shorten :: Lazy.ByteString -> ShortByteString
shorten x = toShort (toStrict x)

-- | Useful when deriving FromCBOR(Annotator T)
-- deriving via (Mem T) instance (Era era) => FromCBOR (Annotator T)
type Mem t = Annotator (MemoBytes t)

showMemo :: Show t => MemoBytes t -> String
showMemo (Memo t b) = "(Memo " ++ show t ++ "  " ++ show b ++ ")"

printMemo :: Show t => MemoBytes t -> IO ()
printMemo x = putStrLn (showMemo x)

memoBytes :: Encode w t -> MemoBytes t
memoBytes t = Memo (runE t) (shorten (toLazyByteString (encode t)))

roundTripMemo :: (FromCBOR t) => MemoBytes t -> Either Codec.CBOR.Read.DeserialiseFailure (Lazy.ByteString, MemoBytes t)
roundTripMemo (Memo _t bytes) =
  case deserialiseFromBytes fromCBOR (fromStrict (fromShort bytes)) of
    Left err -> Left err
    Right (leftover, newt) -> Right (leftover, Memo newt bytes)
