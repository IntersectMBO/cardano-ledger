{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Chain.UTxO.TxAux (
  TxAux,
  ATxAux (..),
  mkTxAux,
  annotateTxAux,
  taTx,
  taWitness,
  txaF,
) where

import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Chain.UTxO.TxWitness (TxWitness)
import Cardano.Ledger.Binary (
  Annotated (..),
  ByteSpan,
  DecCBOR (..),
  Decoded (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  annotatedDecoder,
  byronProtVer,
  decCBORAnnotated,
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  serialize,
  slice,
  toByronCBOR,
  unsafeDeserialize,
 )
import Cardano.Prelude
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy as Lazy
import Formatting (Format, bprint, build, later)
import qualified Formatting.Buildable as B

-- | Transaction + auxiliary data
type TxAux = ATxAux ()

mkTxAux :: Tx -> TxWitness -> TxAux
mkTxAux tx tw = ATxAux (Annotated tx ()) (Annotated tw ()) ()

annotateTxAux :: TxAux -> ATxAux ByteString
annotateTxAux ta = Lazy.toStrict . slice bs <$> ta'
  where
    bs = serialize byronProtVer ta
    ta' = unsafeDeserialize byronProtVer bs

data ATxAux a = ATxAux
  { aTaTx :: !(Annotated Tx a)
  , aTaWitness :: !(Annotated TxWitness a)
  , aTaAnnotation :: !a
  }
  deriving (Generic, Show, Eq, Functor)
  deriving anyclass (NFData)

instance Decoded (ATxAux ByteString) where
  type BaseType (ATxAux ByteString) = ATxAux ()
  recoverBytes = aTaAnnotation

-- Used for debugging purposes only
instance ToJSON a => ToJSON (ATxAux a)

taTx :: ATxAux a -> Tx
taTx = unAnnotated . aTaTx

taWitness :: ATxAux a -> TxWitness
taWitness = unAnnotated . aTaWitness

-- | Specialized formatter for 'TxAux'
txaF :: Format r (TxAux -> r)
txaF = later $ \ta ->
  bprint
    (build . "\n" . "witnesses: " . listJsonIndent 4)
    (taTx ta)
    (taWitness ta)

instance B.Buildable TxAux where
  build = bprint txaF

instance ToCBOR TxAux where
  toCBOR = toByronCBOR

instance FromCBOR TxAux where
  fromCBOR = fromByronCBOR

instance EncCBOR TxAux where
  encCBOR ta = encodeListLen 2 <> encCBOR (taTx ta) <> encCBOR (taWitness ta)

  encodedSizeExpr size pxy = 1 + size (taTx <$> pxy) + size (taWitness <$> pxy)

instance DecCBOR TxAux where
  decCBOR = void <$> decCBOR @(ATxAux ByteSpan)

instance FromCBOR (ATxAux ByteSpan) where
  fromCBOR = fromByronCBOR

instance DecCBOR (ATxAux ByteSpan) where
  decCBOR = do
    Annotated (tx, witness) byteSpan <- annotatedDecoder $ do
      enforceSize "TxAux" 2
      tx <- decCBORAnnotated
      witness <- decCBORAnnotated
      pure (tx, witness)
    pure $ ATxAux tx witness byteSpan
