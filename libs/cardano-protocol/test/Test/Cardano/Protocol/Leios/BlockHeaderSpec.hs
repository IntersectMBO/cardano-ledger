{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Leios.BlockHeaderSpec (spec) where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (encCBOR),
  decodeFullAnnotator,
  encodeBreak,
  encodeFixedSized,
  encodeListLenIndef,
  encodeNullStrictMaybe,
  natVersion,
  serialize,
 )
import Cardano.Ledger.MemoBytes (mkMemoized)
import Cardano.Protocol.Crypto (KES, StandardCrypto)
import Cardano.Protocol.Leios.BlockHeader (
  Header,
  HeaderBody (..),
  HeaderRaw (HeaderRaw),
  headerBody,
  headerSig,
 )
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy (Proxy (..))
import Test.Cardano.Ledger.Common
import Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary ()

newtype IndefiniteLength = IndefiniteLength (HeaderBody StandardCrypto)

instance EncCBOR IndefiniteLength where
  encCBOR (IndefiniteLength HeaderBody {..}) =
    encodeListLenIndef
      <> encCBOR hbBlockNo
      <> encCBOR hbSlotNo
      <> encCBOR hbPrev
      <> encCBOR hbVk
      <> encodeFixedSized hbVrfVk
      <> encCBOR hbVrfRes
      <> encCBOR hbBodySize
      <> encCBOR hbBodyHash
      <> encCBOR hbOCert
      <> encCBOR hbVersionInfo
      <> encCBOR hbBlockBodyContainsLeiosCert
      <> encodeNullStrictMaybe encCBOR hbEbReferencesAnnouncement
      <> encodeBreak

spec :: Spec
spec =
  describe "Leios header KES signature over an encoded header body" $
    forM_ [natVersion @12 .. maxBound] $ \version ->
      describe (show version) $
        forM_ [("canonical", encCBOR), ("indefinite length", encCBOR . IndefiniteLength)] $
          \(encodingName, encodeBody) ->
            prop encodingName $ \(body :: HeaderBody StandardCrypto) seed -> do
              period <- chooseBoundedIntegral (0, KES.totalPeriodsKES (Proxy @(KES StandardCrypto)) - 1)
              pure $ ioProperty $ do
                let initialSignKey = KES.unsoundPureGenKeyKES $ mkSeedFromBytes $ BS.pack seed
                    verificationKey = KES.unsoundPureDeriveVerKeyKES initialSignKey
                signKey <-
                  expectJust $ foldM (KES.unsoundPureUpdateKES ()) initialSignKey $ takeWhile (< period) [0 ..]
                let bodyBytes = serialize version $ encodeBody body
                signedBody <-
                  expectRight $
                    decodeFullAnnotator
                      version
                      "HeaderBody"
                      (decCBOR @(Annotator (HeaderBody StandardCrypto)))
                      bodyBytes
                let header :: Header StandardCrypto
                    header =
                      mkMemoized version $
                        HeaderRaw signedBody $
                          KES.SignedKES $
                            KES.unsoundPureSignKES () period (BSL.toStrict bodyBytes) signKey
                decodedHeader <-
                  expectRight $
                    decodeFullAnnotator version "Header" (decCBOR @(Annotator (Header StandardCrypto))) $
                      serialize version header
                KES.verifySignedKES () verificationKey period (headerBody decodedHeader) (headerSig decodedHeader)
                  `shouldBe` Right ()
