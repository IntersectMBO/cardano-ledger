{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Protocol.Binary.RoundTrip (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.BHeader (BHeader)
import Data.Typeable
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip
import Test.Cardano.Protocol.TPraos.Arbitrary ()

spec :: Spec
spec = do
  describe "RoundTrip" $ do
    roundTripBlock @ShelleyEra
    roundTripBlock @AllegraEra
    roundTripBlock @MaryEra
    roundTripBlock @AlonzoEra
    roundTripBlock @BabbageEra
    roundTripBlock @ConwayEra

roundTripBlock ::
  forall era.
  ( EraSegWits era
  , Arbitrary (Tx era)
  ) =>
  Spec
roundTripBlock =
  prop (show (typeRep $ Proxy @(Block (BHeader StandardCrypto) era))) $
    withMaxSuccess 3 $
      roundTripAnnEraExpectation @era @(Block (BHeader StandardCrypto) era)
