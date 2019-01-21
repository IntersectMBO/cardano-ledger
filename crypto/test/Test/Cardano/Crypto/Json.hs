{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Json
  (tests)
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property)
import qualified Hedgehog as H

import Test.Cardano.Crypto.Example (exampleProtocolMagic0,
    exampleProtocolMagic1, exampleProtocolMagic2,
    exampleProtocolMagic3, exampleProtocolMagic4)

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `ProtocolMagic` JSON format, the `RequiresNetworkMagic` field defaults to
-- `RequiresMagic`.

goldenProtocolMagic0AesonDec :: Property
goldenProtocolMagic0AesonDec = goldenTestJSONDec
  exampleProtocolMagic0
  "test/golden/json/ProtocolMagic0_Legacy_HasNetworkMagic"

goldenProtocolMagic1AesonDec :: Property
goldenProtocolMagic1AesonDec = goldenTestJSONDec
  exampleProtocolMagic1
  "test/golden/json/ProtocolMagic1_Legacy_HasNetworkMagic"

goldenProtocolMagic2AesonDec :: Property
goldenProtocolMagic2AesonDec = goldenTestJSONDec
  exampleProtocolMagic2
  "test/golden/json/ProtocolMagic2_Legacy_HasNetworkMagic"

-- Legacy JSON encoding where requiresNetworkMagic was
-- encoded as "NMMustBeNothing" or "NMMustBeJust"

goldenProtocolMagic3AesonDec_NMMustBeJust :: Property
goldenProtocolMagic3AesonDec_NMMustBeJust = goldenTestJSONDec
  exampleProtocolMagic3
  "test/golden/json/ProtocolMagic_Legacy_NMMustBeJust"

goldenProtocolMagic4AesonDec_NMMustBeNothing :: Property
goldenProtocolMagic4AesonDec_NMMustBeNothing = goldenTestJSONDec
  exampleProtocolMagic4
  "test/golden/json/ProtocolMagic_Legacy_NMMustBeNothing"

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
