{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Metadata (
  Metadatum (..),
) where

import Cardano.Ledger.Binary (
  DecCBOR (decCBOR),
  Decoder,
  DecoderError (..),
  EncCBOR (encCBOR),
  Encoding,
  TokenType (..),
  cborError,
  decodeBreakOr,
  decodeBytesIndef,
  decodeInteger,
  decodeListLen,
  decodeListLenIndef,
  decodeMapLen,
  decodeMapLenIndef,
  decodeString,
  decodeStringIndef,
  encodeInteger,
  encodeListLen,
  encodeMapLen,
  encodeString,
  getDecoderVersion,
  natVersion,
  peekTokenType,
 )
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData (rnf))
import Control.Monad (when)
import Data.Array.Byte (ByteArray (..))
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | A generic metadatum type.
data Metadatum
  = Map ![(Metadatum, Metadatum)]
  | List ![Metadatum]
  | I !Integer
  | B !ByteArray
  | S !T.Text
  deriving stock (Show, Eq, Ord, Generic)

instance NoThunks Metadatum

instance NFData Metadatum where
  rnf = \case
    Map m -> rnf m
    List l -> rnf l
    I _ -> ()
    B _ -> ()
    S _ -> ()

instance EncCBOR Metadatum where
  encCBOR = encodeMetadatum

instance DecCBOR Metadatum where
  decCBOR = decodeMetadatum

-------------------------------------------------------------------------------
-- CBOR encoding and decoding

encodeMetadatum :: Metadatum -> Encoding
encodeMetadatum (I n) = encodeInteger n
encodeMetadatum (B ba) = encCBOR ba
encodeMetadatum (S s) = encodeString s
encodeMetadatum (List xs) =
  encodeListLen (fromIntegral (length xs))
    <> mconcat
      [ encodeMetadatum x
      | x <- xs
      ]
encodeMetadatum (Map kvs) =
  encodeMapLen (fromIntegral (length kvs))
    <> mconcat
      [ encodeMetadatum k <> encodeMetadatum v
      | (k, v) <- kvs
      ]

-- | Decode a transaction matadatum value from its CBOR representation.
--
-- We do not require canonical representations, just like everywhere else
-- on the chain. We accept both definite and indefinite representations.
--
-- The byte and string length checks are enforced in this decoder as per
-- the CDDL spec.
--
-- starting with Allegra era we enforce the length of strings and bytestrings
-- to be no more than 64 bytes
decodeMetadatum :: Decoder s Metadatum
decodeMetadatum = do
  dv <- getDecoderVersion
  let checkSizes = dv > natVersion @2
  tkty <- peekTokenType
  case tkty of
    -- We support -(2^64) .. 2^64-1, but not big integers
    -- not even big integer representation of values within range
    TypeUInt -> I <$> decodeInteger
    TypeUInt64 -> I <$> decodeInteger
    TypeNInt -> I <$> decodeInteger
    TypeNInt64 -> I <$> decodeInteger
    TypeBytes -> do
      !ba <- decCBOR
      when (checkSizes && Prim.sizeofByteArray ba > 64) $
        decodeError "bytes .size (0..64): bytestring exceeds 64 bytes"
      return (B ba)
    TypeBytesIndef -> do
      decodeBytesIndef
      !ba <- decodeBytesIndefLen []
      when (checkSizes && Prim.sizeofByteArray ba > 64) $
        decodeError "bytes .size (0..64): bytestring exceeds 64 bytes"
      return (B ba)
    TypeString -> do
      !x <- decodeString
      when (checkSizes && TF.lengthWord8 x > 64) $
        decodeError "text .size (0..64): text exceeds 64 bytes"
      return (S x)
    TypeStringIndef -> do
      decodeStringIndef
      !x <- decodeStringIndefLen []
      when (checkSizes && TF.lengthWord8 x > 64) $
        decodeError "text .size (0..64): text exceeds 64 bytes"
      return (S x)

    -- Why does it work to do the same thing here for 32 and 64bit list len
    -- tokens? On 32bit systems the decodeListLen will fail if the value
    -- really is bigger than maxBound :: Int, and on 64bit systems if a value
    -- that big is provided, then it'll fail when it runs out of input for
    -- such a big list. Hence we can do exactly the same for the 32bit and
    -- 64bit cases.
    TypeListLen -> do
      n <- decodeListLen
      xs <- decodeListN n []
      return (List xs)
    TypeListLen64 -> do
      n <- decodeListLen
      xs <- decodeListN n []
      return (List xs)
    TypeListLenIndef -> do
      decodeListLenIndef
      xs <- decodeListIndefLen []
      return (List xs)

    -- Same logic applies as above for large lists.
    TypeMapLen -> do
      n <- decodeMapLen
      xs <- decodeMapN n []
      return (Map xs)
    TypeMapLen64 -> do
      n <- decodeMapLen
      xs <- decodeMapN n []
      return (Map xs)
    TypeMapLenIndef -> do
      decodeMapLenIndef
      xs <- decodeMapIndefLen []
      return (Map xs)
    _ -> decodeError ("Unsupported token type " <> T.pack (show tkty))
  where
    decodeError msg = cborError (DecoderErrorCustom "metadata" msg)

decodeBytesIndefLen :: [ByteArray] -> Decoder s ByteArray
decodeBytesIndefLen acc = do
  stop <- decodeBreakOr
  if stop
    then return $! mconcat $ reverse acc
    else do
      !ba <- decCBOR
      decodeBytesIndefLen (ba : acc)

decodeStringIndefLen :: [T.Text] -> Decoder s T.Text
decodeStringIndefLen acc = do
  stop <- decodeBreakOr
  if stop
    then return $! T.concat (reverse acc)
    else do
      !str <- decodeString
      decodeStringIndefLen (str : acc)

decodeListN :: Int -> [Metadatum] -> Decoder s [Metadatum]
decodeListN !n acc =
  case n of
    0 -> return $! reverse acc
    _ -> do
      !t <- decodeMetadatum
      decodeListN (n - 1) (t : acc)

decodeListIndefLen :: [Metadatum] -> Decoder s [Metadatum]
decodeListIndefLen acc = do
  stop <- decodeBreakOr
  if stop
    then return $! reverse acc
    else do
      !tm <- decodeMetadatum
      decodeListIndefLen (tm : acc)

decodeMapN :: Int -> [(Metadatum, Metadatum)] -> Decoder s [(Metadatum, Metadatum)]
decodeMapN !n acc =
  case n of
    0 -> return $! reverse acc
    _ -> do
      !tm <- decodeMetadatum
      !tm' <- decodeMetadatum
      decodeMapN (n - 1) ((tm, tm') : acc)

decodeMapIndefLen :: [(Metadatum, Metadatum)] -> Decoder s [(Metadatum, Metadatum)]
decodeMapIndefLen acc = do
  stop <- decodeBreakOr
  if stop
    then return $! reverse acc
    else do
      !tm <- decodeMetadatum
      !tm' <- decodeMetadatum
      decodeMapIndefLen ((tm, tm') : acc)
