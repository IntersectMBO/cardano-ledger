{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Leios.BlockHeaderSpec (spec) where

import qualified Cardano.Crypto.KES as KES
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  decodeFullAnnotator,
  encodeTerm,
  natVersion,
  serialize,
 )
import Cardano.Ledger.MemoBytes (mkMemoized)
import Cardano.Protocol.Crypto (KES, StandardCrypto)
import Cardano.Protocol.Leios.BlockHeader (
  Header,
  HeaderBody,
  HeaderRaw (HeaderRaw),
  headerBody,
  headerSig,
 )
import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Test.Cardano.Ledger.Binary.Twiddle (toTerm, twiddle)
import Test.Cardano.Ledger.Common
import Test.Cardano.Protocol.Leios.BlockHeader.Arbitrary ()

spec :: Spec
spec =
  describe "Leios header KES signature over a twiddled header body" $
    forM_ [natVersion @12 .. maxBound] $ \version ->
      prop (show version) $ \(body :: HeaderBody StandardCrypto) seed -> do
        twiddledBody <- twiddle version $ toTerm version body
        period <- chooseBoundedIntegral (0, KES.totalPeriodsKES (Proxy @(KES StandardCrypto)) - 1)
        pure $ ioProperty $ do
          let initialSignKey = KES.unsoundPureGenKeyKES $ mkSeedFromBytes $ BS.pack seed
              verificationKey = KES.unsoundPureDeriveVerKeyKES initialSignKey
          signKey <-
            expectJust $ foldM (KES.unsoundPureUpdateKES ()) initialSignKey $ takeWhile (< period) [0 ..]
          signedBody <-
            expectRight $
              decodeFullAnnotator version "HeaderBody" (decCBOR @(Annotator (HeaderBody StandardCrypto))) $
                serialize version $
                  encodeTerm twiddledBody
          let header :: Header StandardCrypto
              header = mkMemoized version $ HeaderRaw signedBody $ KES.unsoundPureSignedKES () period signedBody signKey
          decodedHeader <-
            expectRight $
              decodeFullAnnotator version "Header" (decCBOR @(Annotator (Header StandardCrypto))) $
                serialize version header
          KES.verifySignedKES () verificationKey period (headerBody decodedHeader) (headerSig decodedHeader)
            `shouldBe` Right ()
