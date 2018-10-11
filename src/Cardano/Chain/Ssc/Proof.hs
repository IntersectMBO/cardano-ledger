{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Cardano.Chain.Ssc.Proof
       ( SscProof (..)
       , mkSscProof
       , VssCertificatesHash
       , dropSscProof
       ) where

import           Cardano.Prelude

import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable as B (Buildable (..))

import           Cardano.Binary.Class (Bi (..), DecoderError (..), Dropper,
                     decodeListLenCanonical, dropBytes, encodeListLen,
                     matchSize)
import           Cardano.Chain.Common (StakeholderId)
import           Cardano.Chain.Ssc.CommitmentsMap (CommitmentsMap)
import           Cardano.Chain.Ssc.OpeningsMap (OpeningsMap)
import           Cardano.Chain.Ssc.Payload (SscPayload (..))
import           Cardano.Chain.Ssc.SharesMap (SharesMap)
import           Cardano.Chain.Ssc.VssCertificate (VssCertificate)
import           Cardano.Chain.Ssc.VssCertificatesMap (VssCertificatesMap (..))
import           Cardano.Crypto (Hash, hash)


-- Note: we can't use 'VssCertificatesMap', because we serialize it as
-- a 'HashSet', but in the very first version of mainnet this map was
-- serialized as a 'HashMap' (and 'VssCertificatesMap' was just a type
-- alias for that 'HashMap').
--
-- Alternative approach would be to keep 'instance Bi VssCertificatesMap'
-- the same as it was in mainnet.
type VssCertificatesHash = Hash (Map StakeholderId VssCertificate)

-- | Proof that SSC payload is correct (it's included into block header)
data SscProof
  = CommitmentsProof !(Hash CommitmentsMap) !VssCertificatesHash
  | OpeningsProof !(Hash OpeningsMap) !VssCertificatesHash
  | SharesProof !(Hash SharesMap) !VssCertificatesHash
  | CertificatesProof !VssCertificatesHash
  deriving (Eq, Show, Generic)

instance B.Buildable SscProof where
    build = \case
        CommitmentsProof comms certs ->
            bprint ("<CommitmentsProof: "%build%","%build%">") comms certs
        OpeningsProof comms certs ->
            bprint ("<OpeningsProof: "%build%","%build%">") comms certs
        SharesProof comms certs ->
            bprint ("<SharesProof: "%build%","%build%">") comms certs
        CertificatesProof certs ->
            bprint ("<CertificatesProof: "%build%">") certs

instance NFData SscProof

-- | Create proof (for inclusion into block header) from 'SscPayload'.
mkSscProof :: SscPayload -> SscProof
mkSscProof payload =
    case payload of
        CommitmentsPayload comms certs ->
            proof CommitmentsProof comms certs
        OpeningsPayload openings certs ->
            proof OpeningsProof openings certs
        SharesPayload shares certs     ->
            proof SharesProof shares certs
        CertificatesPayload certs      ->
            CertificatesProof (hash $ getVssCertificatesMap certs)
  where
    proof constr hm (getVssCertificatesMap -> certs) =
        constr (hash hm) (hash certs)

instance Bi SscProof where
  encode payload = case payload of
    CommitmentsProof comms certs ->
      encodeListLen 3 <> encode (0 :: Word8) <> encode comms <> encode certs
    OpeningsProof comms certs ->
      encodeListLen 3 <> encode (1 :: Word8) <> encode comms <> encode certs
    SharesProof comms certs ->
      encodeListLen 3 <> encode (2 :: Word8) <> encode comms <> encode certs
    CertificatesProof certs ->
      encodeListLen 2 <> encode (3 :: Word8) <> encode certs

  decode = do
    actualLen <- decodeListLenCanonical
    decode >>= \case
      0 -> do
        matchSize "CommitmentsProof" 3 actualLen
        CommitmentsProof <$> decode <*> decode
      1 -> do
        matchSize "OpeningsProof" 3 actualLen
        OpeningsProof <$> decode <*> decode
      2 -> do
        matchSize "SharesProof" 3 actualLen
        SharesProof <$> decode <*> decode
      3 -> do
        matchSize "CertificatesProof" 2 actualLen
        CertificatesProof <$> decode
      t -> cborError $ DecoderErrorUnknownTag "SscProof" t

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
