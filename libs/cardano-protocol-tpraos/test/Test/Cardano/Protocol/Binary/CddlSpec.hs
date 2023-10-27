{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Protocol.Binary.CddlSpec (spec) where

import Cardano.Ledger.Binary.Group (CBORGroup)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Shelley
import Cardano.Protocol.TPraos.BHeader (BHBody, BHeader)
import Cardano.Protocol.TPraos.OCert (OCert)
import qualified Data.ByteString.Lazy as BSL
import Test.Cardano.Ledger.Binary.Cddl (
  beforeAllCddlFile,
  cddlRoundTripAnnCborSpec,
  cddlRoundTripCborSpec,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Shelley.Binary.Cddl (readShelleyCddlFiles)

spec :: Spec
spec =
  describe "CDDL" $ do
    let n = 3
    specForEra @Shelley readShelleyCddlFiles n

specForEra :: forall era. Era era => IO [BSL.ByteString] -> Int -> Spec
specForEra readCddlFiles n = do
  describe (eraName @era) $
    beforeAllCddlFile n readCddlFiles $ do
      let v = eraProtVerLow @era
      cddlRoundTripAnnCborSpec @(BHeader StandardCrypto) v "header"
      cddlRoundTripCborSpec @(BHBody StandardCrypto) v "header_body"
      cddlRoundTripCborSpec @(CBORGroup (OCert StandardCrypto)) v "[ operational_cert ]"
