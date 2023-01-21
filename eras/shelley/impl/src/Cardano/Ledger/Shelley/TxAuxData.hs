{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.TxAuxData (
  Metadatum (..),
  ShelleyTxAuxData (ShelleyTxAuxData),
  hashShelleyTxAuxData,
  validMetadatum,

  -- * Deprecations
  Metadata,
)
where

import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash (..))
import Cardano.Ledger.Binary (
  Annotator (..),
  Decoder,
  DecoderError (..),
  Encoding,
  FromCBOR (fromCBOR),
  ToCBOR (toCBOR),
  TokenType (..),
  cborError,
  decodeBreakOr,
  decodeBytes,
  decodeBytesIndef,
  decodeInteger,
  decodeListLen,
  decodeListLenIndef,
  decodeMapLen,
  decodeMapLenIndef,
  decodeString,
  decodeStringIndef,
  encodeBytes,
  encodeInteger,
  encodeListLen,
  encodeMapLen,
  encodeString,
  peekTokenType,
  serializeEncoding,
  withSlice,
 )
import qualified Cardano.Ledger.Binary.Plain as Plain (EncCBOR (encCBOR), encodePreEncoded)
import Cardano.Ledger.Core (Era (..), EraTxAuxData (..), eraProtVerLow)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes (EraIndependentTxAuxData)
import Cardano.Ledger.SafeHash (
  HashAnnotated,
  SafeHash,
  SafeToHash (..),
  hashAnnotated,
 )
import Cardano.Ledger.Shelley.Era
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Proxy (..), Typeable)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | A generic metadatum type.
data Metadatum
  = Map ![(Metadatum, Metadatum)]
  | List ![Metadatum]
  | I !Integer
  | B !BS.ByteString
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

data ShelleyTxAuxData era = ShelleyTxAuxData'
  { mdMap :: Map Word64 Metadatum
  , mdBytes :: LBS.ByteString
  }
  deriving (Eq, Show, Ord, Generic)
  deriving (NoThunks) via AllowThunksIn '["mdBytes"] (ShelleyTxAuxData era)

type Metadata era = ShelleyTxAuxData era

{-# DEPRECATED Metadata "Use `ShelleyTxAuxData` instead" #-}

instance CC.Crypto c => EraTxAuxData (ShelleyEra c) where
  type TxAuxData (ShelleyEra c) = ShelleyTxAuxData (ShelleyEra c)

  validateTxAuxData _ (ShelleyTxAuxData m) = all validMetadatum m
  hashTxAuxData metadata =
    AuxiliaryDataHash (makeHashWithExplicitProxys (Proxy @c) index metadata)
    where
      index = Proxy @EraIndependentTxAuxData

instance NFData (ShelleyTxAuxData era) where
  rnf m = mdMap m `deepseq` rnf (mdBytes m)

-- Usually we derive SafeToHash instances, but since ShelleyTxAuxData preserves its serialisation
-- bytes we can just extract them here, and make an explicit SafeToHash instance.

instance SafeToHash (ShelleyTxAuxData era) where
  originalBytes = LBS.toStrict . mdBytes

instance c ~ EraCrypto era => HashAnnotated (ShelleyTxAuxData era) EraIndependentTxAuxData c

hashShelleyTxAuxData :: Era era => ShelleyTxAuxData era -> SafeHash (EraCrypto era) EraIndependentTxAuxData
hashShelleyTxAuxData = hashAnnotated

pattern ShelleyTxAuxData :: forall era. Era era => Map Word64 Metadatum -> ShelleyTxAuxData era
pattern ShelleyTxAuxData m <-
  ShelleyTxAuxData' m _
  where
    ShelleyTxAuxData m =
      let bytes = serializeEncoding (eraProtVerLow @era) $ toCBOR m
       in ShelleyTxAuxData' m bytes

{-# COMPLETE ShelleyTxAuxData #-}

instance Typeable era => Plain.EncCBOR (ShelleyTxAuxData era) where
  encCBOR = Plain.encodePreEncoded . LBS.toStrict . mdBytes

instance Typeable era => FromCBOR (Annotator (ShelleyTxAuxData era)) where
  fromCBOR = do
    (m, bytesAnn) <- withSlice fromCBOR
    pure $ ShelleyTxAuxData' m <$> bytesAnn

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

encodeMetadatum :: Metadatum -> Encoding
encodeMetadatum (I n) = encodeInteger n
encodeMetadatum (B b) = encodeBytes b
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
  tkty <- peekTokenType
  case tkty of
    -- We support -(2^64-1) .. 2^64-1, but not big integers
    -- not even big integer representation of values within range
    TypeUInt -> I <$> decodeInteger
    TypeUInt64 -> I <$> decodeInteger
    TypeNInt -> I <$> decodeInteger
    TypeNInt64 -> I <$> decodeInteger
    -- Note that we do not enforce byte and string lengths here in the
    -- decoder. We enforce that in the tx validation rules.
    TypeBytes -> do
      !x <- decodeBytes
      return (B x)
    TypeBytesIndef -> do
      decodeBytesIndef
      !x <- decodeBytesIndefLen []
      return (B x)
    TypeString -> do
      !x <- decodeString
      return (S x)
    TypeStringIndef -> do
      decodeStringIndef
      !x <- decodeStringIndefLen []
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

decodeBytesIndefLen :: [BS.ByteString] -> Decoder s ByteString
decodeBytesIndefLen acc = do
  stop <- decodeBreakOr
  if stop
    then return $! BS.concat (reverse acc)
    else do
      !bs <- decodeBytes
      decodeBytesIndefLen (bs : acc)

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
