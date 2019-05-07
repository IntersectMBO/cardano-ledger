{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module Cardano.Chain.UTxO.Tx
  ( Tx(..)
  , txF
  , TxId
  , TxAttributes
  , TxIn(..)
  , TxOut(..)
  )
where

import Cardano.Prelude

import Data.Aeson
  ( FromJSON(..)
  , FromJSONKey(..)
  , FromJSONKeyFunction(..)
  , ToJSON(toJSON)
  , ToJSONKey(..)
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Text as T
import Formatting (Format, bprint, build, builder, int, sformat)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Case(..)
  , DecoderError(DecoderErrorUnknownTag)
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeKnownCborDataItem
  , encodeKnownCborDataItem
  , encodeListLen
  , enforceSize
  , knownCborDataItemSizeExpr
  , szCases
  )
import Cardano.Chain.Common
  ( Address(..)
  , Lovelace
  , fromCBORTextAddress
  , integerToLovelace
  , lovelaceF
  , lovelaceToInteger
  )
import Cardano.Chain.Common.Attributes (Attributes, attributesAreKnown)
import Cardano.Crypto (Hash, decodeAbstractHash, hash, hashHexF, shortHashF)


--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | Transaction
--
--   NB: transaction witnesses are stored separately
data Tx = UnsafeTx
  { txInputs     :: !(NonEmpty TxIn)
  -- ^ Inputs of transaction.
  , txOutputs    :: !(NonEmpty TxOut)
  -- ^ Outputs of transaction.
  , txAttributes :: !TxAttributes
  -- ^ Attributes of transaction
  } deriving (Eq, Ord, Generic, Show)
    deriving anyclass NFData

instance B.Buildable Tx where
  build tx = bprint
    ( "Tx "
    . build
    . " with inputs "
    . listJson
    . ", outputs: "
    . listJson
    . builder
    )
    (hash tx)
    (txInputs tx)
    (txOutputs tx)
    attrsBuilder
   where
    attrs = txAttributes tx
    attrsBuilder
      | attributesAreKnown attrs = mempty
      | otherwise                = bprint (", attributes: " . build) attrs

instance ToCBOR Tx where
  toCBOR tx =
    encodeListLen 3 <> toCBOR (txInputs tx) <> toCBOR (txOutputs tx) <> toCBOR
      (txAttributes tx)

  encodedSizeExpr size pxy =
    1 + size (txInputs <$> pxy) + size (txOutputs <$> pxy) + size
      (txAttributes <$> pxy)

instance FromCBOR Tx where
  fromCBOR = do
    enforceSize "Tx" 3
    UnsafeTx <$> fromCBOR <*> fromCBOR <*> fromCBOR

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
  -- | TxId = Which transaction's output is used
  -- | Word32 = Index of the output in transaction's outputs
  = TxInUtxo TxId Word32
  deriving (Eq, Ord, Generic, Show)
  deriving anyclass NFData

instance FromJSON TxIn where
  parseJSON v = toAesonError =<< txInFromText <$> parseJSON v

instance ToJSON TxIn where
  toJSON = toJSON . txInToText

instance FromJSONKey TxIn where
  fromJSONKey = FromJSONKeyTextParser (toAesonError . txInFromText)

instance ToJSONKey TxIn where
  toJSONKey = toJSONKeyText txInToText

instance B.Buildable TxIn where
  build (TxInUtxo txInHash txInIndex) =
    bprint ("TxInUtxo " . shortHashF . " #" . int) txInHash txInIndex

instance ToCBOR TxIn where
  toCBOR (TxInUtxo txInHash txInIndex) =
    encodeListLen 2 <> toCBOR (0 :: Word8) <> encodeKnownCborDataItem
      (txInHash, txInIndex)

  encodedSizeExpr size _ = 2 + knownCborDataItemSizeExpr
    (szCases [Case "TxInUtxo" $ size $ Proxy @(TxId, Word32)])

instance FromCBOR TxIn where
  fromCBOR = do
    enforceSize "TxIn" 2
    tag <- fromCBOR @Word8
    case tag of
      0 -> uncurry TxInUtxo <$> decodeKnownCborDataItem
      _ -> cborError $ DecoderErrorUnknownTag "TxIn" tag

instance HeapWords TxIn where
  heapWords (TxInUtxo txid w32) = heapWords2 txid w32

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
  ["TxInUtxo", h, idx] ->
    TxInUtxo <$> decodeAbstractHash h <*> first toS (readEither (toS idx))
  _ -> Left $ "Invalid TxIn " <> t

txInToText :: TxIn -> Text
txInToText (TxInUtxo txInHash txInIndex) =
  sformat ("TxInUtxo_" . hashHexF . "_" . int) txInHash txInIndex


--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output
data TxOut = TxOut
  { txOutAddress :: !Address
  , txOutValue   :: !Lovelace
  } deriving (Eq, Ord, Generic, Show)
    deriving anyclass NFData

instance FromJSON TxOut where
  parseJSON = withObject "TxOut" $ \o ->
    TxOut
      <$> (toAesonError . fromCBORTextAddress =<< o .: "address")
      <*> (toAesonError . integerToLovelace =<< o .: "lovelace")

instance ToJSON TxOut where
  toJSON txOut = object
    [ "lovelace" .= lovelaceToInteger (txOutValue txOut)
    , "address" .= sformat build (txOutAddress txOut)
    ]

instance B.Buildable TxOut where
  build txOut = bprint
    ("TxOut " . lovelaceF . " -> " . build)
    (txOutValue txOut)
    (txOutAddress txOut)

instance ToCBOR TxOut where
  toCBOR txOut =
    encodeListLen 2 <> toCBOR (txOutAddress txOut) <> toCBOR (txOutValue txOut)

  encodedSizeExpr size pxy =
    1 + size (txOutAddress <$> pxy) + size (txOutValue <$> pxy)

instance FromCBOR TxOut where
  fromCBOR = do
    enforceSize "TxOut" 2
    TxOut <$> fromCBOR <*> fromCBOR

instance HeapWords TxOut where
  heapWords (TxOut address _) = 3 + heapWords address

deriveJSON defaultOptions ''Tx
