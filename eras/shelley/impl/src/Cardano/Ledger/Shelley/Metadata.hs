{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Metadata
  ( Metadatum (..),
    Metadata (Metadata),
    hashMetadata,
    validMetadatum,
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
import Cardano.Ledger.Hashes (EraIndependentAuxiliaryData)
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash (..),
    hashAnnotated,
  )
import Cardano.Ledger.Serialization (mapFromCBOR, mapToCBOR)
import Cardano.Prelude (cborError)
import Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | A generic metadatum type.
data Metadatum
  = -- TODO make strict:
    Map [(Metadatum, Metadatum)]
  | List [Metadatum]
  | I !Integer
  | B !BS.ByteString
  | S !T.Text
  deriving stock (Show, Eq, Ord, Generic)

instance NoThunks Metadatum

data Metadata era = Metadata'
  { mdMap :: Map Word64 Metadatum,
    mdBytes :: LBS.ByteString
  }
  deriving (Eq, Show, Ord, Generic)
  deriving (NoThunks) via AllowThunksIn '["mdBytes"] (Metadata era)

-- Usually we derive SafetToHash instances, but since Metadata preserves its serialisation
-- bytes we can just extract them here, and make an explicit SafeToHash instance.

instance SafeToHash (Metadata era) where
  originalBytes = LBS.toStrict . mdBytes

instance c ~ Crypto era => HashAnnotated (Metadata era) EraIndependentAuxiliaryData c

hashMetadata :: Era era => Metadata era -> SafeHash (Crypto era) EraIndependentAuxiliaryData
hashMetadata = hashAnnotated

pattern Metadata :: Map Word64 Metadatum -> Metadata era
pattern Metadata m <-
  Metadata' m _
  where
    Metadata m =
      let bytes = serializeEncoding $ mapToCBOR m
       in Metadata' m bytes

{-# COMPLETE Metadata #-}

instance Typeable era => ToCBOR (Metadata era) where
  toCBOR = encodePreEncoded . LBS.toStrict . mdBytes

instance Typeable era => FromCBOR (Annotator (Metadata era)) where
  fromCBOR = do
    (m, bytesAnn) <- withSlice mapFromCBOR
    pure $ Metadata' m <$> bytesAnn

instance ToCBOR Metadatum where
  toCBOR = encodeMetadatum

instance FromCBOR Metadatum where
  fromCBOR = decodeMetadatum

-- Validation of sizes

validMetadatum :: Metadatum -> Bool
-- The integer size/representation checks are enforced in the decoder.
validMetadatum (I _) = True
validMetadatum (B b) = BS.length b <= 64
validMetadatum (S s) = BS.length (T.encodeUtf8 s) <= 64
validMetadatum (List xs) = all validMetadatum xs
validMetadatum (Map kvs) =
  all
    ( \(k, v) ->
        validMetadatum k
          && validMetadatum v
    )
    kvs

-------------------------------------------------------------------------------
-- CBOR encoding and decoding

encodeMetadatum :: Metadatum -> CBOR.Encoding
encodeMetadatum (I n) = CBOR.encodeInteger n
encodeMetadatum (B b) = CBOR.encodeBytes b
encodeMetadatum (S s) = CBOR.encodeString s
encodeMetadatum (List xs) =
  CBOR.encodeListLen (fromIntegral (length xs))
    <> mconcat
      [ encodeMetadatum x
        | x <- xs
      ]
encodeMetadatum (Map kvs) =
  CBOR.encodeMapLen (fromIntegral (length kvs))
    <> mconcat
      [ encodeMetadatum k <> encodeMetadatum v
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
decodeMetadatum :: Decoder s Metadatum
decodeMetadatum = do
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

decodeListN :: Int -> [Metadatum] -> Decoder s [Metadatum]
decodeListN !n acc =
  case n of
    0 -> return $! reverse acc
    _ -> do
      !t <- decodeMetadatum
      decodeListN (n -1) (t : acc)

decodeListIndefLen :: [Metadatum] -> Decoder s [Metadatum]
decodeListIndefLen acc = do
  stop <- CBOR.decodeBreakOr
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
      decodeMapN (n -1) ((tm, tm') : acc)

decodeMapIndefLen :: [(Metadatum, Metadatum)] -> Decoder s [(Metadatum, Metadatum)]
decodeMapIndefLen acc = do
  stop <- CBOR.decodeBreakOr
  if stop
    then return $! reverse acc
    else do
      !tm <- decodeMetadatum
      !tm' <- decodeMetadatum
      decodeMapIndefLen ((tm, tm') : acc)
