{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PatternSynonyms     #-}

module Cardano.Chain.UTxO.TxWitness
  ( TxWitness(txInWitnesses, txWitnessSerialized)
  , pattern TxWitness
  , TxInWitness(..)
  , TxSigData(..)
  , TxSig
  , recoverSigData
  )
where

import Cardano.Prelude as P

import Data.Vector (Vector)
import Formatting (bprint, build)
import qualified Formatting.Buildable as B

import Cardano.Binary
  ( Annotated(..)
  , Case(..)
  , DecoderError(DecoderErrorUnknownTag)
  , FromCBOR(..)
  , FromCBORAnnotated(..)
  , ToCBOR(..)
  , decodeListLen
  , encodeListLen
  , encodePreEncoded
  , matchSize
  , serialize'
  , szCases
  , withSlice'
  , serializeEncoding'
  )
import Cardano.Chain.Common.CBOR
  (encodeKnownCborDataItem, knownCborDataItemSizeExpr, decodeKnownCborDataItem)
import Cardano.Chain.Common (addressHash)
import Cardano.Chain.UTxO.Tx (Tx)
import Cardano.Crypto
  ( Hash
  , VerificationKey
  , RedeemVerificationKey
  , RedeemSignature
  , Signature
  , hash
  , shortHashF
  )


-- | A witness is a proof that a transaction is allowed to spend the funds it
--   spends (by providing signatures, redeeming scripts, etc). A separate proof
--   is provided for each input.
data TxWitness = TxWitness'
  { txInWitnesses' :: !(Vector TxInWitness)
  , txWitnessSerialized :: ByteString
  } deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance ToCBOR TxWitness where
  toCBOR = encodePreEncoded . txWitnessSerialized

  encodedSizeExpr size _ = encodedSizeExpr size (Proxy :: Proxy (Vector TxInWitness))

pattern TxWitness :: Vector TxInWitness -> TxWitness
pattern TxWitness{ txInWitnesses } <- TxWitness' txInWitnesses _
  where
  TxWitness wits = TxWitness' wits (serializeEncoding' $ toCBOR wits)

instance FromCBORAnnotated TxWitness where
  fromCBORAnnotated' = withSlice' . lift $
    TxWitness' <$> fromCBOR

-- | A witness for a single input
data TxInWitness
  = VKWitness !VerificationKey !TxSig
  -- ^ VKWitness twKey twSig
  | RedeemWitness !RedeemVerificationKey !(RedeemSignature TxSigData)
  -- ^ RedeemWitness twRedeemKey twRedeemSig
  deriving (Eq, Show, Generic)
  deriving anyclass NFData

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

recoverSigData :: Tx -> Annotated TxSigData ByteString
recoverSigData tx =
  let
    txHash      = hash tx
    signedBytes = serialize' txHash --TODO: make the prefix bytes explicit
  in Annotated (TxSigData txHash) signedBytes

instance ToCBOR TxSigData where
  toCBOR txSigData = toCBOR (txSigTxHash txSigData)
  encodedSizeExpr size pxy = size (txSigTxHash <$> pxy)

instance FromCBOR TxSigData where
  fromCBOR = TxSigData <$> fromCBOR

-- | 'Signature' of addrId
type TxSig = Signature TxSigData
