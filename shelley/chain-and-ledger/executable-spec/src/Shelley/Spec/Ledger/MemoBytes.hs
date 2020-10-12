{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | MemoBytes is an abstration for a datetype that encodes its own seriialization.
--   The idea is to use a newtype around a MemoBytes non-memoizing version.
--   For example:   newtype Foo = Foo(MemoBytes NonMemoizingFoo)
--   This way all the instances for Foo (Eq,Show,Ord,ToCBOR,FromCBOR,NoThunks,Generic)
--   can be derived for free.
module Shelley.Spec.Ledger.MemoBytes
  ( MemoBytes (..),
    Symbolic (..),
    (<@>),
    (<#>),
    Mem,
    encodeSym,
    runSym,
    memoBytes,
    shorten,
    decodeList,
    decodeSeq,
    decodeStrictSeq,
    decodeSet,
    encodeList,
    encodeSeq,
    encodeStrictSeq,
    encodeSet,
    showMemo,
    printMemo,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
    encodePreEncoded,
    encodeWord,
    withSlice,
  )
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Write (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as Lazy
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.Serialization
  ( decodeList,
    decodeSeq,
    decodeSet,
    decodeStrictSeq,
    encodeFoldable,
  )
import Prelude hiding (span)

-- ========================================================================

data MemoBytes t = Memo {memotype :: !t, memobytes :: {-# UNPACK #-} !ShortByteString}
  deriving (NoThunks) via AllowThunksIn '["memobytes"] (MemoBytes t)

deriving instance Generic (MemoBytes t)

instance (Typeable t) => ToCBOR (MemoBytes t) where
  toCBOR (Memo _ bytes) = encodePreEncoded (fromShort bytes)

instance (Typeable t, FromCBOR (Annotator t)) => FromCBOR (Annotator (MemoBytes t)) where
  fromCBOR = do
    (Annotator getT, Annotator getBytes) <- withSlice fromCBOR
    pure (Annotator (\fullbytes -> Memo (getT fullbytes) (toShort (toStrict (getBytes fullbytes)))))

instance Eq t => Eq (MemoBytes t) where (Memo x _) == (Memo y _) = x == y

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

-- ===============================================================================
-- A Symbolic encoding is a data structure from which 3 things can be recovered
-- Given:    x :: Symbolic t
-- 1) get a value of type t
-- 2) get an Encoding for that value. (Care must be taken that the tags are correct)
-- 3) get a (MemoBytes t)
-- The advantage of using Symbolic with a MemoBytes, is we don't have to make a ToCBOR
-- instance. Instead the "instance" is spread amongst the pattern constuctors by using
-- (memoBytes symbolic) in the where clause of the pattern contructor.
-- See some examples in the EXAMPLE Section below.

data Symbolic t where
  Con :: t -> Word -> Symbolic t
  App :: ToCBOR a => Symbolic (a -> t) -> a -> Symbolic t
  AppE :: Symbolic (a -> t) -> (a -> Encoding, a) -> Symbolic t

infixl 5 <@>

infixl 5 <#>

(<@>) :: ToCBOR a => Symbolic (a -> t) -> a -> Symbolic t
x <@> y = App x y

(<#>) :: Symbolic (a -> t) -> (a -> Encoding, a) -> Symbolic t
x <#> y = AppE x y

runSym :: Symbolic t -> t
runSym (Con constr _tag) = constr
runSym (App f x) = runSym f x
runSym (AppE f (_e, x)) = runSym f x

encodeSym :: Symbolic t -> Encoding
encodeSym sym = encodeHelp 1 sym
  where
    encodeHelp :: Word -> Symbolic t -> Encoding
    encodeHelp n (Con _constr tag) = encodeListLen n <> encodeWord tag
    encodeHelp n (App f x) = encodeHelp (n + 1) f <> toCBOR x
    encodeHelp n (AppE f (encode, x)) = encodeHelp (n + 1) f <> encode x

memoBytes :: Symbolic t -> MemoBytes t
memoBytes t = Memo (runSym t) (shorten (toLazyByteString (encodeSym t)))

-- ===========================================================================================
-- These functions are the analogs to
-- Shelley.Spec.Ledger.Serialization(decodeList, decodeSeq, decodeStrictSeq, decodeSet)
-- It is not well documented how to use encodeFoldable.
-- They are provided here as compatible pairs for use with the symbolic constructor AppE (<#>)
-- If we use decodeX in the fromCBOR we must use encodeX in the ToCOBOR or (Symbolic t) values

encodeList :: ToCBOR a => [a] -> Encoding
encodeList = encodeFoldable

encodeStrictSeq :: ToCBOR a => StrictSeq a -> Encoding
encodeStrictSeq = encodeFoldable

encodeSeq :: ToCBOR a => Seq a -> Encoding
encodeSeq = encodeFoldable

encodeSet :: ToCBOR a => Set a -> Encoding
encodeSet = encodeFoldable

-- ===========================================
-- For a worked out EXAMPLE see the testfile:
-- cardano-ledger-specs/shelley/chain-and-ledger/shelley-spec-ledger-test/test/Test/Shelley/Spec/Ledger/MemoBytes.hs
