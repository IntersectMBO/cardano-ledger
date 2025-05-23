{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Chain.Ssc (
  SscPayload (..),
  dropSscPayload,
  SscProof (..),
  dropSscProof,
  dropCommitmentsMap,
  dropSignedCommitment,
  dropCommitment,
  dropOpeningsMap,
  dropSharesMap,
  dropInnerSharesMap,
  dropVssCertificatesMap,
  dropVssCertificate,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecoderError (..),
  Dropper,
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
  cborError,
  decodeListLen,
  dropBytes,
  dropList,
  dropMap,
  dropSet,
  dropTriple,
  dropWord64,
  encodeListLen,
  enforceSize,
  fromByronCBOR,
  matchSize,
  toByronCBOR,
 )
import Cardano.Prelude hiding (cborError)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as ByteString (pack)
import NoThunks.Class (NoThunks (..))

--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

data SscPayload
  = SscPayload
  deriving (Eq, Show, Generic, NFData)

instance ToCBOR SscPayload where
  toCBOR = toByronCBOR

instance FromCBOR SscPayload where
  fromCBOR = fromByronCBOR

-- Used for debugging purposes only
instance ToJSON SscPayload

instance EncCBOR SscPayload where
  encCBOR _ =
    encodeListLen 2
      <> encCBOR (3 :: Word8)
      <> encCBOR (mempty :: Set ())

instance DecCBOR SscPayload where
  decCBOR = do
    dropSscPayload
    pure SscPayload

dropSscPayload :: Dropper s
dropSscPayload = do
  actualLen <- decodeListLen
  decCBOR >>= \case
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

data SscProof
  = SscProof
  deriving (Eq, Show, Generic, NFData, NoThunks)

instance ToCBOR SscProof where
  toCBOR = toByronCBOR

instance FromCBOR SscProof where
  fromCBOR = fromByronCBOR

-- Used for debugging purposes only
instance ToJSON SscProof

instance EncCBOR SscProof where
  encCBOR _ =
    encodeListLen 2 <> encCBOR (3 :: Word8) <> encCBOR hashBytes
    where
      -- The VssCertificatesMap is encoded as a HashSet, so you'd think we want
      -- the hash of the encoding of an empty HashSet. BUT NO! For the calculation
      -- of the hashes in the header, it uses the encoding of the underlying
      -- HashMap. The hash of the encoded empty HashMap is
      --   d36a2619a672494604e11bb447cbcf5231e9f2ba25c2169177edc941bd50ad6c
      hashBytes :: ByteString
      hashBytes =
        ByteString.pack
          [ 0xd3
          , 0x6a
          , 0x26
          , 0x19
          , 0xa6
          , 0x72
          , 0x49
          , 0x46
          , 0x04
          , 0xe1
          , 0x1b
          , 0xb4
          , 0x47
          , 0xcb
          , 0xcf
          , 0x52
          , 0x31
          , 0xe9
          , 0xf2
          , 0xba
          , 0x25
          , 0xc2
          , 0x16
          , 0x91
          , 0x77
          , 0xed
          , 0xc9
          , 0x41
          , 0xbd
          , 0x50
          , 0xad
          , 0x6c
          ]

  encodedSizeExpr size _ =
    1
      + encodedSizeExpr size (Proxy :: Proxy Word8)
      + 34

instance DecCBOR SscProof where
  decCBOR = do
    dropSscProof
    pure SscProof

dropSscProof :: Dropper s
dropSscProof = do
  actualLen <- decodeListLen
  decCBOR >>= \case
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
  -- EpochNumber
  dropWord64
  -- Signature (AsBinary VssVerificationKey, EpochNumber)
  dropBytes
  -- VerificationKey
  dropBytes
