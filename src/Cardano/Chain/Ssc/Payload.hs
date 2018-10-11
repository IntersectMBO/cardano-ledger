{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}

module Cardano.Chain.Ssc.Payload
       ( SscPayload (..)
       , checkSscPayload
       , spVss
       , dropSscPayload
       ) where

import           Cardano.Prelude hiding (id)

import           Control.Monad.Except (MonadError)
import qualified Data.Map.Strict as Map
import           Data.Text.Lazy.Builder (Builder)
import           Formatting (Format, bprint, build, int, (%))
import qualified Formatting.Buildable as B (Buildable (..))

import           Cardano.Binary.Class (Bi (..), DecoderError (..), Dropper,
                     decodeListLenCanonical, encodeListLen, matchSize)
import           Cardano.Chain.Ssc.CommitmentsMap
import           Cardano.Chain.Ssc.OpeningsMap
import           Cardano.Chain.Ssc.SharesMap
import           Cardano.Chain.Ssc.VssCertificate
                     (VssCertificate (vcExpiryEpoch))
import           Cardano.Chain.Ssc.VssCertificatesMap
import           Cardano.Crypto (ProtocolMagic)


-- | Payload included into blocks.
data SscPayload
    = CommitmentsPayload !CommitmentsMap !VssCertificatesMap
    | OpeningsPayload !OpeningsMap !VssCertificatesMap
    | SharesPayload !SharesMap !VssCertificatesMap
    | CertificatesPayload !VssCertificatesMap
    deriving (Eq, Show, Generic)
    deriving anyclass NFData

instance B.Buildable SscPayload where
  build gp
    | isEmptySscPayload gp = "  no SSC payload"
    | otherwise = case gp of
      CommitmentsPayload comms    certs -> formatTwo formatCommitments comms certs
      OpeningsPayload    openings certs -> formatTwo formatOpenings openings certs
      SharesPayload      shares   certs -> formatTwo formatShares shares certs
      CertificatesPayload certs         -> formatCertificates certs
   where
    formatIfNotNull
      :: Container c => Format Builder (c -> Builder) -> c -> Builder
    formatIfNotNull formatter l
      | null l    = mempty
      | otherwise = bprint formatter l
    formatCommitments (getCommitmentsMap -> comms) = formatIfNotNull
      ("  commitments from: " % listJson % "\n")
      (Map.keys comms)
    formatOpenings openings = formatIfNotNull
      ("  openings from: " % listJson % "\n")
      (Map.keys openings)
    formatShares shares =
      formatIfNotNull ("  shares from: " % listJson % "\n") (Map.keys shares)
    formatCertificates (getVssCertificatesMap -> certs) = formatIfNotNull
      ("  certificates from: " % listJson % "\n")
      (map formatVssCert $ Map.toList certs)
    formatVssCert (id, cert) =
      bprint (build % ":" % int) id (vcExpiryEpoch cert)
    formatTwo formatter hm certs =
      mconcat [formatter hm, formatCertificates certs]

instance Bi SscPayload where
    encode payload = case payload of
        CommitmentsPayload comms certs ->
            encodeListLen 3
                <> encode (0 :: Word8)
                <> encode comms
                <> encode certs
        OpeningsPayload comms certs ->
            encodeListLen 3
                <> encode (1 :: Word8)
                <> encode comms
                <> encode certs
        SharesPayload comms certs ->
            encodeListLen 3
                <> encode (2 :: Word8)
                <> encode comms
                <> encode certs
        CertificatesPayload certs ->
            encodeListLen 2
                <> encode (3 :: Word8)
                <> encode certs

    decode = do
        actualLen <- decodeListLenCanonical
        decode >>= \case
            0 -> do
                matchSize "CommitmentsPayload" 3 actualLen
                CommitmentsPayload <$> decode <*> decode
            1 -> do
                matchSize "OpeningsPayload" 3 actualLen
                OpeningsPayload <$> decode <*> decode
            2 -> do
                matchSize "SharesPayload" 3 actualLen
                SharesPayload <$> decode <*> decode
            3 -> do
                matchSize "CertificatesPayload" 2 actualLen
                CertificatesPayload <$> decode
            t -> cborError $ DecoderErrorUnknownTag "SscPayload" t

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

isEmptySscPayload :: SscPayload -> Bool
isEmptySscPayload (CommitmentsPayload comms certs) = null comms && null certs
isEmptySscPayload (OpeningsPayload opens certs)    = null opens && null certs
isEmptySscPayload (SharesPayload shares certs)     = null shares && null certs
isEmptySscPayload (CertificatesPayload certs)      = null certs

spVss :: SscPayload -> VssCertificatesMap
spVss (CommitmentsPayload _ vss) = vss
spVss (OpeningsPayload    _ vss) = vss
spVss (SharesPayload      _ vss) = vss
spVss (CertificatesPayload  vss) = vss

checkSscPayload :: MonadError Text m => ProtocolMagic -> SscPayload -> m ()
checkSscPayload pm payload = checkVssCertificatesMap pm (spVss payload)
