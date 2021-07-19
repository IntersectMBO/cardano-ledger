{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Json
  ( tests,
  )
where

import Cardano.Prelude
import Hedgehog (Property)
import qualified Hedgehog as H
import Test.Cardano.Crypto.Example
  ( exampleProtocolMagic3,
    exampleProtocolMagic4,
  )
import Test.Cardano.Prelude

--------------------------------------------------------------------------------
-- ProtocolMagic
--------------------------------------------------------------------------------

-- Legacy JSON encoding where requiresNetworkMagic was
-- encoded as "NMMustBeNothing" or "NMMustBeJust"

goldenProtocolMagic3AesonDec_NMMustBeJust :: Property
goldenProtocolMagic3AesonDec_NMMustBeJust =
  goldenTestJSONDec
    exampleProtocolMagic3
    "test/golden/json/ProtocolMagic_Legacy_NMMustBeJust"

goldenProtocolMagic4AesonDec_NMMustBeNothing :: Property
goldenProtocolMagic4AesonDec_NMMustBeNothing =
  goldenTestJSONDec
    exampleProtocolMagic4
    "test/golden/json/ProtocolMagic_Legacy_NMMustBeNothing"

tests :: IO Bool
tests = H.checkSequential $$discoverGolden
