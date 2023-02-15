{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Chain.UTxO.Tx (
  Tx (..),
  txF,
  TxId,
  TxAttributes,
  TxIn (..),
  TxOut (..),
)
where

import Cardano.Chain.Common (
  Address (..),
  Lovelace,
  lovelaceF,
 )
import Cardano.Chain.Common.Attributes (Attributes, attributesAreKnown)
import Cardano.Chain.Common.CBOR (
  decodeKnownCborDataItem,
  encodeKnownCborDataItem,
  knownCborDataItemSizeExpr,
 )
import Cardano.Crypto (Hash, serializeCborHash, shortHashF)
import Cardano.HeapWords (HeapWords (..))
import Cardano.Ledger.Binary (
  Case (..),
  DecCBOR (..),
  DecoderError (DecoderErrorUnknownTag),
  EncCBOR (..),
  cborError,
  encodeListLen,
  enforceSize,
  szCases,
 )
import Cardano.Prelude hiding (cborError)
import Data.Aeson (ToJSON)
import Formatting (Format, bprint, build, builder, int)
import qualified Formatting.Buildable as B

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | Transaction
--
--   NB: transaction witnesses are stored separately
data Tx = UnsafeTx
  { txInputs :: !(NonEmpty TxIn)
  -- ^ Inputs of transaction.
  , txOutputs :: !(NonEmpty TxOut)
  -- ^ Outputs of transaction.
  , txAttributes :: !TxAttributes
  -- ^ Attributes of transaction
  }
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance B.Buildable Tx where
  build tx =
    bprint
      ( "Tx "
          . build
          . " with inputs "
          . listJson
          . ", outputs: "
          . listJson
          . builder
      )
      (serializeCborHash tx)
      (txInputs tx)
      (txOutputs tx)
      attrsBuilder
    where
      attrs = txAttributes tx
      attrsBuilder
        | attributesAreKnown attrs = mempty
        | otherwise = bprint (", attributes: " . build) attrs

-- Used for debugging purposes only
instance ToJSON Tx

instance EncCBOR Tx where
  encCBOR tx =
    encodeListLen 3
      <> encCBOR (txInputs tx)
      <> encCBOR (txOutputs tx)
      <> encCBOR
        (txAttributes tx)

  encodedSizeExpr size pxy =
    1
      + size (txInputs <$> pxy)
      + size (txOutputs <$> pxy)
      + size
        (txAttributes <$> pxy)

instance DecCBOR Tx where
  decCBOR = do
    enforceSize "Tx" 3
    UnsafeTx <$> decCBOR <*> decCBOR <*> decCBOR

-- | Specialized formatter for 'Tx'
txF :: Format r (Tx -> r)
txF = build

--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

-- | Represents transaction identifier as 'Hash' of 'Tx'
type TxId = Hash Tx

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

-- | Represents transaction attributes: map from 1-byte integer to
--   arbitrary-type value. To be used for extending transaction with new fields
--   via softfork.
type TxAttributes = Attributes ()

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------

-- | Transaction arbitrary input
data TxIn
  = -- | TxId = Which transaction's output is used
    -- | Word16 = Index of the output in transaction's outputs
    TxInUtxo TxId Word16
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance B.Buildable TxIn where
  build (TxInUtxo txInHash txInIndex) =
    bprint ("TxInUtxo " . shortHashF . " #" . int) txInHash txInIndex

-- Used for debugging purposes only
instance ToJSON TxIn

instance EncCBOR TxIn where
  encCBOR (TxInUtxo txInHash txInIndex) =
    encodeListLen 2
      <> encCBOR (0 :: Word8)
      <> encodeKnownCborDataItem
        (txInHash, txInIndex)

  encodedSizeExpr size _ =
    2
      + knownCborDataItemSizeExpr
        (szCases [Case "TxInUtxo" $ size $ Proxy @(TxId, Word16)])

instance DecCBOR TxIn where
  decCBOR = do
    enforceSize "TxIn" 2
    tag <- decCBOR @Word8
    case tag of
      0 -> uncurry TxInUtxo <$> decodeKnownCborDataItem
      _ -> cborError $ DecoderErrorUnknownTag "TxIn" tag

instance HeapWords TxIn where
  heapWords (TxInUtxo txid _w16) = 3 + heapWords txid + 2

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output
data TxOut = TxOut
  { txOutAddress :: !Address
  , txOutValue :: !Lovelace
  }
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass (NFData)

instance B.Buildable TxOut where
  build txOut =
    bprint
      ("TxOut " . lovelaceF . " -> " . build)
      (txOutValue txOut)
      (txOutAddress txOut)

-- Used for debugging purposes only
instance ToJSON TxOut

instance EncCBOR TxOut where
  encCBOR txOut =
    encodeListLen 2 <> encCBOR (txOutAddress txOut) <> encCBOR (txOutValue txOut)

  encodedSizeExpr size pxy =
    1 + size (txOutAddress <$> pxy) + size (txOutValue <$> pxy)

instance DecCBOR TxOut where
  decCBOR = do
    enforceSize "TxOut" 2
    TxOut <$> decCBOR <*> decCBOR

instance HeapWords TxOut where
  heapWords (TxOut address _) = 3 + heapWords address
