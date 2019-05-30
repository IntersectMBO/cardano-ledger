{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Chain.UTxO.TxWitness
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

import Cardano.Binary
  ( Annotated(..)
  , Case(..)
  , DecoderError(DecoderErrorUnknownTag)
  , FromCBOR(..)
  , ToCBOR(..)
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
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Crypto
  ( Hash
  , VerificationKey
  , RedeemVerificationKey
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
  = VKWitness !VerificationKey !TxSig
  -- ^ VKWitness twKey twSig
  | RedeemWitness !RedeemVerificationKey !(RedeemSignature TxSigData)
  -- ^ RedeemWitness twRedeemKey twRedeemSig
  deriving (Eq, Show, Generic)
  deriving anyclass NFData

instance ToJSON TxInWitness where
  toJSON = \case
    VKWitness twKey twSig ->
      object ["tag" .= ("VKWitness" :: Text), "key" .= twKey, "sig" .= twSig]
    RedeemWitness twRedeemKey twRedeemSig -> object
      [ "tag" .= ("RedeemWitness" :: Text)
      , "redeemKey" .= twRedeemKey
      , "redeemSig" .= twRedeemSig
      ]

instance FromJSON TxInWitness where
  parseJSON = withObject "TxInWitness" $ \o -> (o .: "tag") >>= \case
    ("VKWitness" :: Text) -> VKWitness <$> (o .: "key") <*> (o .: "sig")
    "RedeemWitness" ->
      RedeemWitness <$> (o .: "redeemKey") <*> (o .: "redeemSig")
    _ ->
      aesonError @Text
        "expected 'tag' to be one of 'VKWitness' or 'RedeemWitness'"

instance B.Buildable TxInWitness where
  build (VKWitness key sig) = bprint
    ( "VKWitness: key = "
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
    bprint ("VKWitness: key = " . build . ", sig = " . build) key sig

instance ToCBOR TxInWitness where
  toCBOR input = case input of
    VKWitness key sig ->
      encodeListLen 2
        <> toCBOR (0 :: Word8)
        <> encodeKnownCborDataItem (key, sig)
    RedeemWitness key sig ->
      encodeListLen 2
        <> toCBOR (2 :: Word8)
        <> encodeKnownCborDataItem (key, sig)

  encodedSizeExpr size _ = 2 + szCases
    (map
      (fmap knownCborDataItemSizeExpr)
      [ Case "VKWitness" $ size $ Proxy @(VerificationKey, TxSig)
      , Case "RedeemWitness"
      $ size
      $ Proxy @(RedeemVerificationKey, RedeemSignature TxSigData)
      ]
    )

instance FromCBOR TxInWitness where
  fromCBOR = do
    len <- decodeListLen
    fromCBOR @Word8 >>= \case
      0 -> do
        matchSize "TxInWitness.VKWitness" len 2
        uncurry VKWitness <$> decodeKnownCborDataItem
      2 -> do
        matchSize "TxInWitness.RedeemWitness" len 2
        uncurry RedeemWitness <$> decodeKnownCborDataItem
      tag -> cborError $ DecoderErrorUnknownTag "TxInWitness" tag

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

instance ToCBOR TxSigData where
  toCBOR txSigData = toCBOR (txSigTxHash txSigData)
  encodedSizeExpr size pxy = size (txSigTxHash <$> pxy)

instance FromCBOR TxSigData where
  fromCBOR = TxSigData <$> fromCBOR

-- | 'Signature' of addrId
type TxSig = Signature TxSigData

deriveJSON defaultOptions ''TxSigData
