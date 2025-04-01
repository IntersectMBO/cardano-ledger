{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.BinarySpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Binary (DecCBOR)
import Cardano.Ledger.Block
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Data.Proxy
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Binary (decoderEquivalenceProp, decoderEquivalenceSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Arbitrary ()
import Test.Cardano.Protocol.Binary.Annotator
import Test.Cardano.Protocol.TPraos.Arbitrary ()

spec :: Spec
spec = do
  describe "DecCBOR instances equivalence" $ do
    decoderEquivalenceSpec @(BHeader StandardCrypto) minBound maxBound
    blockEraSpec @ShelleyEra
    blockEraSpec @AllegraEra
    blockEraSpec @MaryEra
    blockEraSpec @AlonzoEra

blockEraSpec ::
  forall era.
  ( EraSegWits era
  , Arbitrary (Tx era)
  , DecCBOR (Annotator (TxSeq era))
  ) =>
  Spec
blockEraSpec =
  prop (show (typeRep $ Proxy @(Block (BHeader StandardCrypto) era))) $
    withMaxSuccess 3 $
      decoderEquivalenceProp @(Block (BHeader StandardCrypto) era)
        (eraProtVerLow @era)
        (eraProtVerHigh @era)
