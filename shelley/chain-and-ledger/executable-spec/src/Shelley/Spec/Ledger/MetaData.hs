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
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (AllowThunksIn (..), LByteString, NoUnexpectedThunks (..), Word64, cborError)
import qualified Codec.CBOR.Term as CBOR
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Keys (Hash, hashWithSerialiser)
import Shelley.Spec.Ledger.Serialization (mapFromCBOR, mapToCBOR)

-- | A generic metadatum type.
--
-- TODO make strict
data MetaDatum
  = Map [(MetaDatum, MetaDatum)]
  | List [MetaDatum]
  | I Integer
  | B B.ByteString
  | S T.Text
  deriving stock (Show, Eq, Ord, Generic)

instance NoUnexpectedThunks MetaDatum

data MetaData = MetaData'
  { mdMap :: Map Word64 MetaDatum,
    mdBytes :: LByteString
  }
  deriving (Eq, Show, Generic)
  deriving (NoUnexpectedThunks) via AllowThunksIn '["mdBytes"] MetaData

pattern MetaData :: Map Word64 MetaDatum -> MetaData
pattern MetaData m <-
  MetaData' m _
  where
    MetaData m =
      let bytes = serializeEncoding $ mapToCBOR m
       in MetaData' m bytes

{-# COMPLETE MetaData #-}

instance ToCBOR MetaData where
  toCBOR = encodePreEncoded . BL.toStrict . mdBytes

instance FromCBOR (Annotator MetaData) where
  fromCBOR = do
    (m, bytesAnn) <- withSlice mapFromCBOR
    pure $ MetaData' m <$> bytesAnn

type CBORToDataError = String

{- Note [Permissive decoding]
We're using a canonical representation of lists, maps, bytes, and integers. However,
the CBOR library does not guarantee that a TInteger gets encoded as a big integer,
so we can't rely on getting back our canonical version when we decode (see
https://github.com/well-typed/cborg/issues/222). So we need to be permissive
when we decode.
-}

fromTerm :: CBOR.Term -> Either CBORToDataError MetaDatum
fromTerm t =
  case t of
    CBOR.TInt i -> Right $ I (fromIntegral i)
    CBOR.TInteger i -> Right $ I i
    CBOR.TBytes b -> Right $ B b
    CBOR.TBytesI b -> Right $ B (BL.toStrict b)
    CBOR.TString s -> Right $ S s
    CBOR.TStringI s -> Right $ S (TL.toStrict s)
    CBOR.TMap m -> Map <$> traverse (bitraverse fromTerm fromTerm) m
    CBOR.TMapI m -> Map <$> traverse (bitraverse fromTerm fromTerm) m
    CBOR.TList l -> List <$> traverse fromTerm l
    CBOR.TListI l -> List <$> traverse fromTerm l
    _ -> Left $ "Unsupported CBOR term: " ++ show t

toTerm :: MetaDatum -> CBOR.Term
toTerm = \case
  I i -> CBOR.TInteger i
  B b -> CBOR.TBytes b
  S s -> CBOR.TString s
  Map entries -> CBOR.TMap (fmap (bimap toTerm toTerm) entries)
  List ts -> CBOR.TList (fmap toTerm ts)

instance ToCBOR MetaDatum where
  toCBOR = CBOR.encodeTerm . toTerm

instance FromCBOR MetaDatum where
  fromCBOR = do
    t <- CBOR.decodeTerm
    case fromTerm t of
      Right d -> pure d
      Left e -> (cborError . DecoderErrorCustom "metadata" . T.pack) e

newtype MetaDataHash era = MetaDataHash {unsafeMetaDataHash :: Hash era MetaData}
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

deriving instance Era era => ToCBOR (MetaDataHash era)

deriving instance Era era => FromCBOR (MetaDataHash era)

hashMetaData ::
  Era era =>
  MetaData ->
  MetaDataHash era
hashMetaData = MetaDataHash . hashWithSerialiser toCBOR
