{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Chain.Txp.Tx
  ( Tx(..)
  , txInputs
  , txOutputs
  , txAttributes
  , txF
  , TxId
  , TxAttributes
  , TxIn(..)
  , isTxInUnknown
  , TxOut(..)
  , _TxOut
  )
where

import Cardano.Prelude

import Control.Lens (makeLenses, makePrisms)
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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Formatting (Format, bprint, build, builder, int, sformat)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Bi(..)
  , Case(..)
  , decodeKnownCborDataItem
  , decodeUnknownCborDataItem
  , encodeKnownCborDataItem
  , encodeListLen
  , encodeUnknownCborDataItem
  , enforceSize
  , knownCborDataItemSizeExpr
  , szCases
  )
import Cardano.Chain.Common
  ( Address(..)
  , Lovelace
  , lovelaceF
  , lovelaceToInteger
  , decodeTextAddress
  , integerToLovelace
  )
import Cardano.Chain.Common.Attributes (Attributes, areAttributesKnown)
import Cardano.Crypto (Hash, decodeAbstractHash, hash, hashHexF, shortHashF)


--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

-- | Transaction
--
--   NB: transaction witnesses are stored separately
data Tx = UnsafeTx
  { _txInputs     :: !(NonEmpty TxIn)
  -- ^ Inputs of transaction.
  , _txOutputs    :: !(NonEmpty TxOut)
  -- ^ Outputs of transaction.
  , _txAttributes :: !TxAttributes
  -- ^ Attributes of transaction
  } deriving (Eq, Ord, Generic, Show)

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
    (_txInputs tx)
    (_txOutputs tx)
    attrsBuilder
   where
    attrs = _txAttributes tx
    attrsBuilder
      | areAttributesKnown attrs = mempty
      | otherwise                = bprint (", attributes: " . build) attrs

instance Bi Tx where
  encode tx =
    encodeListLen 3 <> encode (_txInputs tx) <> encode (_txOutputs tx) <> encode
      (_txAttributes tx)

  decode = do
    enforceSize "Tx" 3
    UnsafeTx <$> decode <*> decode <*> decode

  encodedSizeExpr size pxy =
    1 + size (_txInputs <$> pxy) + size (_txOutputs <$> pxy) + size
      (_txAttributes <$> pxy)

instance NFData Tx

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
  | TxInUnknown !Word8 !ByteString
  deriving (Eq, Ord, Generic, Show)

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
  build (TxInUnknown tag bs) =
    bprint ("TxInUnknown " . int . " " . base16F) tag bs

instance Bi TxIn where
  encode (TxInUtxo txInHash txInIndex) =
    encodeListLen 2 <> encode (0 :: Word8) <> encodeKnownCborDataItem
      (txInHash, txInIndex)
  encode (TxInUnknown tag bs) =
    encodeListLen 2 <> encode tag <> encodeUnknownCborDataItem (LBS.fromStrict bs)

  decode = do
    enforceSize "TxIn" 2
    tag <- decode @Word8
    case tag of
      0 -> uncurry TxInUtxo <$> decodeKnownCborDataItem
      _ -> TxInUnknown tag <$> decodeUnknownCborDataItem

  encodedSizeExpr size _ = 2 + knownCborDataItemSizeExpr
    (szCases [Case "TxInUtxo" $ size $ Proxy @(TxId, Word32)])

instance NFData TxIn

isTxInUnknown :: TxIn -> Bool
isTxInUnknown (TxInUnknown _ _) = True
isTxInUnknown _                 = False

txInFromText :: Text -> Either Text TxIn
txInFromText t = case T.splitOn "_" t of
  ["TxInUtxo", h, idx] ->
    TxInUtxo <$> decodeAbstractHash h <*> first toS (readEither (toS idx))
  ["TxInUnknown", tag, bs] ->
    TxInUnknown <$> first toS (readEither (toS tag)) <*> first
      show
      (parseBase16 bs)
  _ -> Left $ "Invalid TxIn " <> t

txInToText :: TxIn -> Text
txInToText (TxInUtxo txInHash txInIndex) =
  sformat ("TxInUtxo_" . hashHexF . "_" . int) txInHash txInIndex
txInToText (TxInUnknown tag bs) =
  sformat ("TxInUnknown_" . int . "_" . base16F) tag bs


--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

-- | Transaction output
data TxOut = TxOut
  { txOutAddress :: !Address
  , txOutValue   :: !Lovelace
  } deriving (Eq, Ord, Generic, Show)

instance FromJSON TxOut where
  parseJSON = withObject "TxOut" $ \o ->
    TxOut
      <$> (toAesonError . decodeTextAddress =<< o .: "address")
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

instance NFData TxOut

instance Bi TxOut where
  encode txOut =
    encodeListLen 2 <> encode (txOutAddress txOut) <> encode (txOutValue txOut)

  decode = do
    enforceSize "TxOut" 2
    TxOut <$> decode <*> decode

  encodedSizeExpr size pxy =
    1 + size (txOutAddress <$> pxy) + size (txOutValue <$> pxy)

makePrisms ''TxOut

makeLenses ''Tx

deriveJSON defaultOptions ''Tx
