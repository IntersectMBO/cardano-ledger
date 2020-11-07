{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shelley.Spec.Ledger.MetaData
  ( MetaDatum (..),
    MetaData (MetaData),
    MetaDataHash (..),
    hashMetaData,
    validMetaData,
  )
where

import Cardano.Binary
  ( Annotator (..),
    DecoderError (..),
    FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodePreEncoded,
    serializeEncoding,
    withSlice,
  )
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Prelude (cborError)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Shelley.Spec.Ledger.Keys (Hash, hashWithSerialiser)
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)

-- | A generic metadatum type.
data MetaDatum
  = -- TODO make strict:
    Map [(MetaDatum, MetaDatum)]
  | List [MetaDatum]
  | I !Integer
  | B !BS.ByteString
  | S !T.Text
  deriving stock (Show, Eq, Ord, Generic)

instance NoThunks MetaDatum

data MetaData = MetaData'
  { mdMap :: Map Word64 MetaDatum,
    mdBytes :: LBS.ByteString
  }
  deriving (Eq, Show, Generic)
  deriving (NoThunks) via AllowThunksIn '["mdBytes"] MetaData

pattern MetaData :: Map Word64 MetaDatum -> MetaData
pattern MetaData m <-
  MetaData' m _
  where
    MetaData m =
      let bytes = serializeEncoding $ mapToCBOR m
       in MetaData' m bytes

{-# COMPLETE MetaData #-}

instance ToCBOR MetaData where
  toCBOR = encodePreEncoded . LBS.toStrict . mdBytes

instance FromCBOR (Annotator MetaData) where
  fromCBOR = do
    (m, bytesAnn) <- withSlice mapFromCBOR
    pure $ MetaData' m <$> bytesAnn

instance ToCBOR MetaDatum where
  toCBOR = encodeMetaDatum

instance FromCBOR MetaDatum where
  fromCBOR = decodeMetaDatum

newtype MetaDataHash era = MetaDataHash {unsafeMetaDataHash :: Hash (Crypto era) MetaData}
  deriving (Show, Eq, Ord, NoThunks, NFData)

deriving instance Era era => ToCBOR (MetaDataHash era)

deriving instance Era era => FromCBOR (MetaDataHash era)

hashMetaData ::
  Era era =>
  MetaData ->
  MetaDataHash era
hashMetaData = MetaDataHash . hashWithSerialiser toCBOR

--------------------------------------------------------------------------------
-- Validation of sizes

validMetaData :: MetaData -> Bool
validMetaData (MetaData m) = all validMetaDatum m

validMetaDatum :: MetaDatum -> Bool
-- The integer size/representation checks are enforced in the decoder.
validMetaDatum (I _) = True
validMetaDatum (B b) = BS.length b <= 64
validMetaDatum (S s) = BS.length (T.encodeUtf8 s) <= 64
validMetaDatum (List xs) = all validMetaDatum xs
validMetaDatum (Map kvs) =
  all
    ( \(k, v) ->
        validMetaDatum k
          && validMetaDatum v
    )
    kvs

--------------------------------------------------------------------------------
-- CBOR encoding and decoding

encodeMetaDatum :: MetaDatum -> CBOR.Encoding
encodeMetaDatum (I n) = CBOR.encodeInteger n
encodeMetaDatum (B b) = CBOR.encodeBytes b
encodeMetaDatum (S s) = CBOR.encodeString s
encodeMetaDatum (List xs) =
  CBOR.encodeListLen (fromIntegral (length xs))
    <> mconcat
      [ encodeMetaDatum x
        | x <- xs
      ]
encodeMetaDatum (Map kvs) =
  CBOR.encodeMapLen (fromIntegral (length kvs))
    <> mconcat
      [ encodeMetaDatum k <> encodeMetaDatum v
        | (k, v) <- kvs
      ]

-- | Decode a transaction matadatum value from its CBOR representation.
--
-- The CDDL for the CBOR is
--
-- > transaction_metadatum =
-- >     int
-- >   / bytes .size (0..64)
-- >   / text .size (0..64)
-- >   / [ * transaction_metadatum ]
-- >   / { * transaction_metadatum => transaction_metadatum }
--
-- We do not require canonical representations, just like everywhere else
-- on the chain. We accept both definte and indefinite representations.
--
-- The byte and string length checks are not enforced in this decoder, but
decodeMetaDatum :: Decoder s MetaDatum
decodeMetaDatum = do
  tkty <- CBOR.peekTokenType
  case tkty of
    -- We support -(2^64-1) .. 2^64-1, but not big integers
    -- not even big integer representation of values within range
    CBOR.TypeUInt -> I <$> CBOR.decodeInteger
    CBOR.TypeUInt64 -> I <$> CBOR.decodeInteger
    CBOR.TypeNInt -> I <$> CBOR.decodeInteger
    CBOR.TypeNInt64 -> I <$> CBOR.decodeInteger
    -- Note that we do not enforce byte and string lengths here in the
    -- decoder. We enforce that in the tx validation rules.
    CBOR.TypeBytes -> do
      !x <- CBOR.decodeBytes
      return (B x)
    CBOR.TypeBytesIndef -> do
      CBOR.decodeBytesIndef
      !x <- decodeBytesIndefLen []
      return (B x)
    CBOR.TypeString -> do
      !x <- CBOR.decodeString
      return (S x)
    CBOR.TypeStringIndef -> do
      CBOR.decodeStringIndef
      !x <- decodeStringIndefLen []
      return (S x)

    -- Why does it work to do the same thing here for 32 and 64bit list len
    -- tokens? On 32bit systems the decodeListLen will fail if the value
    -- really is bigger than maxBound :: Int, and on 64bit systems if a value
    -- that big is provided, then it'll fail when it runs out of input for
    -- such a big list. Hence we can do exactly the same for the 32bit and
    -- 64bit cases.
    CBOR.TypeListLen -> do
      n <- CBOR.decodeListLen
      xs <- decodeListN n []
      return (List xs)
    CBOR.TypeListLen64 -> do
      n <- CBOR.decodeListLen
      xs <- decodeListN n []
      return (List xs)
    CBOR.TypeListLenIndef -> do
      CBOR.decodeListLenIndef
      xs <- decodeListIndefLen []
      return (List xs)

    -- Same logic applies as above for large lists.
    CBOR.TypeMapLen -> do
      n <- CBOR.decodeMapLen
      xs <- decodeMapN n []
      return (Map xs)
    CBOR.TypeMapLen64 -> do
      n <- CBOR.decodeMapLen
      xs <- decodeMapN n []
      return (Map xs)
    CBOR.TypeMapLenIndef -> do
      CBOR.decodeMapLenIndef
      xs <- decodeMapIndefLen []
      return (Map xs)
    _ -> decodeError ("Unsupported CBOR token type " <> T.pack (show tkty))
  where
    decodeError msg = cborError (DecoderErrorCustom "metadata" msg)

decodeBytesIndefLen :: [BS.ByteString] -> CBOR.Decoder s ByteString
decodeBytesIndefLen acc = do
  stop <- CBOR.decodeBreakOr
  if stop
    then return $! BS.concat (reverse acc)
    else do
      !bs <- CBOR.decodeBytes
      decodeBytesIndefLen (bs : acc)

decodeStringIndefLen :: [T.Text] -> Decoder s T.Text
decodeStringIndefLen acc = do
  stop <- CBOR.decodeBreakOr
  if stop
    then return $! T.concat (reverse acc)
    else do
      !str <- CBOR.decodeString
      decodeStringIndefLen (str : acc)

decodeListN :: Int -> [MetaDatum] -> Decoder s [MetaDatum]
decodeListN !n acc =
  case n of
    0 -> return $! reverse acc
    _ -> do
      !t <- decodeMetaDatum
      decodeListN (n -1) (t : acc)

decodeListIndefLen :: [MetaDatum] -> Decoder s [MetaDatum]
decodeListIndefLen acc = do
  stop <- CBOR.decodeBreakOr
  if stop
    then return $! reverse acc
    else do
      !tm <- decodeMetaDatum
      decodeListIndefLen (tm : acc)

decodeMapN :: Int -> [(MetaDatum, MetaDatum)] -> Decoder s [(MetaDatum, MetaDatum)]
decodeMapN !n acc =
  case n of
    0 -> return $! reverse acc
    _ -> do
      !tm <- decodeMetaDatum
      !tm' <- decodeMetaDatum
      decodeMapN (n -1) ((tm, tm') : acc)

decodeMapIndefLen :: [(MetaDatum, MetaDatum)] -> Decoder s [(MetaDatum, MetaDatum)]
decodeMapIndefLen acc = do
  stop <- CBOR.decodeBreakOr
  if stop
    then return $! reverse acc
    else do
      !tm <- decodeMetaDatum
      !tm' <- decodeMetaDatum
      decodeMapIndefLen ((tm, tm') : acc)
