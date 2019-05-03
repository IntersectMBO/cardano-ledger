{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Ssc
  ( SscPayload(..)
  , dropSscPayload
  , SscProof(..)
  , dropSscProof
  , dropCommitmentsMap
  , dropSignedCommitment
  , dropCommitment
  , dropOpeningsMap
  , dropSharesMap
  , dropInnerSharesMap
  , dropVssCertificatesMap
  , dropVssCertificate
  )
where

import Cardano.Prelude

import Cardano.Binary
  ( DecoderError(..)
  , Dropper
  , FromCBOR(..)
  , ToCBOR(..)
  , decodeListLen
  , dropBytes
  , dropList
  , dropMap
  , dropSet
  , dropTriple
  , dropWord64
  , encodeListLen
  , enforceSize
  , matchSize
  )


--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

data SscPayload =
  SscPayload
  deriving (Eq, Show, Generic, NFData)

instance ToCBOR SscPayload where
  toCBOR _ = encodeListLen 2
    <> toCBOR (3 :: Word8)
    <> toCBOR (mempty :: Set ())

instance FromCBOR SscPayload where
  fromCBOR = do
    dropSscPayload
    pure SscPayload

dropSscPayload :: Dropper s
dropSscPayload = do
  actualLen <- decodeListLen
  fromCBOR >>= \case
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


--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

data SscProof =
  SscProof
  deriving (Eq, Show, Generic, NFData)

instance ToCBOR SscProof where
  toCBOR _ =
    encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR (mempty :: ByteString)

instance FromCBOR SscProof where
  fromCBOR = do
    dropSscProof
    pure SscProof

dropSscProof :: Dropper s
dropSscProof = do
  actualLen <- decodeListLen
  fromCBOR >>= \case
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


--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

dropCommitmentsMap :: Dropper s
dropCommitmentsMap = dropSet dropSignedCommitment

dropSignedCommitment :: Dropper s
dropSignedCommitment = dropTriple dropBytes dropCommitment dropBytes

dropCommitment :: Dropper s
dropCommitment = do
  enforceSize "Commitment" 2
  -- Map (AsBinary VssVerificationKey) (NonEmpty (AsBinary EncShare))
  dropMap dropBytes (dropList dropBytes)
  dropSecretProof

dropSecretProof :: Dropper s
dropSecretProof = do
  enforceSize "SecretProof" 4
  -- Scrape.ExtraGen
  dropBytes
  -- Scrape.Proof
  dropBytes
  -- Scrape.ParallelProofs
  dropBytes
  -- [Scrape.Commitment]
  dropList dropBytes


--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

dropOpeningsMap :: Dropper s
dropOpeningsMap = dropMap dropBytes dropBytes


--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

dropSharesMap :: Dropper s
dropSharesMap = dropMap dropBytes dropInnerSharesMap

dropInnerSharesMap :: Dropper s
dropInnerSharesMap = dropMap dropBytes (dropList dropBytes)


--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

dropVssCertificatesMap :: Dropper s
dropVssCertificatesMap = dropSet dropVssCertificate

dropVssCertificate :: Dropper s
dropVssCertificate = do
  enforceSize "VssCertificate" 4
  -- AsBinary VssVerificationKey
  dropBytes
  -- EpochIndex
  dropWord64
  -- Signature (AsBinary VssVerificationKey, EpochIndex)
  dropBytes
  -- VerificationKey
  dropBytes
