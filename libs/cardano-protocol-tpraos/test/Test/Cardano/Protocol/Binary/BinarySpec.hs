{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.BinarySpec (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
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
import Test.Cardano.Protocol.Binary.Annotator ()
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
  ( EraBlockBody era
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  , Arbitrary (Tx era)
#endif
  , Arbitrary (BlockBody era)
  ) =>
  Spec
blockEraSpec =
  prop (show (typeRep $ Proxy @(Block (BHeader StandardCrypto) era))) $
    withMaxSuccess 3 $
      decoderEquivalenceProp @(Block (BHeader StandardCrypto) era)
        (eraProtVerLow @era)
        (eraProtVerHigh @era)
