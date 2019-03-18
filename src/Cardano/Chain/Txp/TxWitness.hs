{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Chain.Txp.TxWitness
  ( TxWitness
  , TxInWitness(..)
  , TxSigData(..)
  , TxSig
  , recoverSigData
  )
where

import Cardano.Prelude

import Data.Aeson (FromJSON(..), ToJSON(toJSON), object, withObject, (.:), (.=))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Vector (Vector)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary.Class
  ( Annotated(..)
  , Bi(..)
  , Case(..)
  , DecoderError(DecoderErrorUnknownTag)
  , decodeKnownCborDataItem
  , decodeListLen
  , encodeKnownCborDataItem
  , encodeListLen
  , knownCborDataItemSizeExpr
  , matchSize
  , serialize'
  , szCases
  )
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.Txp.Tx (Tx)
import Cardano.Crypto
  ( Hash
  , PublicKey
  , RedeemPublicKey
  , RedeemSignature
  , Signature
  , hashDecoded
  , shortHashF
  )


-- | A witness is a proof that a transaction is allowed to spend the funds it
--   spends (by providing signatures, redeeming scripts, etc). A separate proof
--   is provided for each input.
type TxWitness = Vector TxInWitness

-- | A witness for a single input
data TxInWitness
  = PkWitness !PublicKey !TxSig
  -- ^ PkWitness twKey twSig
  | RedeemWitness !RedeemPublicKey !(RedeemSignature TxSigData)
  -- ^ RedeemWitness twRedeemKey twRedeemSig
  deriving (Eq, Show, Generic)
  deriving anyclass NFData

instance ToJSON TxInWitness where
  toJSON = \case
    PkWitness twKey twSig ->
      object ["tag" .= ("PkWitness" :: Text), "key" .= twKey, "sig" .= twSig]
    RedeemWitness twRedeemKey twRedeemSig -> object
      [ "tag" .= ("RedeemWitness" :: Text)
      , "redeemKey" .= twRedeemKey
      , "redeemSig" .= twRedeemSig
      ]

instance FromJSON TxInWitness where
  parseJSON = withObject "TxInWitness" $ \o -> (o .: "tag") >>= \case
    ("PkWitness" :: Text) -> PkWitness <$> (o .: "key") <*> (o .: "sig")
    "RedeemWitness" ->
      RedeemWitness <$> (o .: "redeemKey") <*> (o .: "redeemSig")
    _ ->
      aesonError @Text
        "expected 'tag' to be one of 'PkWitness' or 'RedeemWitness'"

instance B.Buildable TxInWitness where
  build (PkWitness key sig) = bprint
    ( "PkWitness: key = "
    . build
    . ", key hash = "
    . shortHashF
    . ", sig = "
    . build
    )
    key
    (addressHash key)
    sig
  build (RedeemWitness key sig) =
    bprint ("PkWitness: key = " . build . ", sig = " . build) key sig

instance Bi TxInWitness where
  encode input = case input of
    PkWitness key sig ->
      encodeListLen 2
        <> encode (0 :: Word8)
        <> encodeKnownCborDataItem (key, sig)
    RedeemWitness key sig ->
      encodeListLen 2
        <> encode (2 :: Word8)
        <> encodeKnownCborDataItem (key, sig)

  decode = do
    len <- decodeListLen
    decode @Word8 >>= \case
      0 -> do
        matchSize "TxInWitness.PkWitness" len 2
        uncurry PkWitness <$> decodeKnownCborDataItem
      2 -> do
        matchSize "TxInWitness.RedeemWitness" len 2
        uncurry RedeemWitness <$> decodeKnownCborDataItem
      tag -> cborError $ DecoderErrorUnknownTag "TxInWitness" tag

  encodedSizeExpr size _ = 2 + szCases
    (map
      (fmap knownCborDataItemSizeExpr)
      [ Case "PkWitness" $ size $ Proxy @(PublicKey, TxSig)
      , Case "RedeemWitness"
      $ size
      $ Proxy @(RedeemPublicKey, RedeemSignature TxSigData)
      ]
    )

-- | Data that is being signed when creating a TxSig
newtype TxSigData = TxSigData
  { txSigTxHash :: Hash Tx
  } deriving (Eq, Show, Generic)

recoverSigData :: Annotated Tx ByteString -> Annotated TxSigData ByteString
recoverSigData atx =
  let
    txHash      = hashDecoded atx
    signedBytes = serialize' txHash --TODO: make the prefix bytes explicit
  in Annotated (TxSigData txHash) signedBytes

instance Bi TxSigData where
  encode txSigData = encode (txSigTxHash txSigData)
  decode = TxSigData <$> decode
  encodedSizeExpr size pxy = size (txSigTxHash <$> pxy)

-- | 'Signature' of addrId
type TxSig = Signature TxSigData

deriveJSON defaultOptions ''TxSigData
