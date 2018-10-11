{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Ssc
       ( module X
       , SscPayload (..)
       , SscProof (..)
       , dropSscPayload
       , dropSscProof
       ) where

import           Cardano.Chain.Ssc.Commitment as X
import           Cardano.Chain.Ssc.CommitmentsMap as X
import           Cardano.Chain.Ssc.Opening as X
import           Cardano.Chain.Ssc.OpeningsMap as X
import           Cardano.Chain.Ssc.SharesMap as X
import           Cardano.Chain.Ssc.VssCertificate as X
import           Cardano.Chain.Ssc.VssCertificatesMap as X

import           Cardano.Prelude

import           Cardano.Binary.Class (Bi (..), DecoderError (..), Dropper,
                     decodeListLenCanonical, dropBytes, encodeListLen,
                     matchSize)

data SscPayload =
  SscPayload
  deriving (Eq, Show, Generic, NFData)

instance Bi SscPayload where
  encode _ = encodeListLen 2
    <> encode (3 :: Word8)
    <> encode (mempty :: Set ())

  decode = do
    dropSscPayload
    pure SscPayload

dropSscPayload :: Dropper s
dropSscPayload = do
  actualLen <- decodeListLenCanonical
  decode >>= \case
    0 -> do
      matchSize "CommitmentsPayload" 3 actualLen
      dropCommitmentsMap
      dropVssCertificatesMap
    1 -> do
      matchSize "OpeningsPayload" 3 actualLen
      dropOpeningsMap
      dropVssCertificatesMap
    2 -> do
      matchSize "SharesPayload" 3 actualLen
      dropSharesMap
      dropVssCertificatesMap
    3 -> do
      matchSize "CertificatesPayload" 2 actualLen
      dropVssCertificatesMap
    t -> cborError $ DecoderErrorUnknownTag "SscPayload" t

data SscProof =
  SscProof
  deriving (Eq, Show, Generic, NFData)

instance Bi SscProof where
  encode _ =
    encodeListLen 2 <> encode (3 :: Word8) <> encode (mempty :: ByteString)

  decode = do
    dropSscProof
    pure SscProof

dropSscProof :: Dropper s
dropSscProof = do
  actualLen <- decodeListLenCanonical
  decode >>= \case
    0 -> do
      matchSize "CommitmentsProof" 3 actualLen
      dropBytes
      dropBytes
    1 -> do
      matchSize "OpeningsProof" 3 actualLen
      dropBytes
      dropBytes
    2 -> do
      matchSize "SharesProof" 3 actualLen
      dropBytes
      dropBytes
    3 -> do
      matchSize "CertificatesProof" 2 actualLen
      dropBytes
    t -> cborError $ DecoderErrorUnknownTag "SscProof" t
