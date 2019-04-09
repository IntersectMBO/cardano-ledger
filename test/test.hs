module Main
  ( main
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Options.Applicative (execParser)

import Test.Options (Opts(..), optsParser)

import qualified Test.Cardano.Chain.Block.CBOR
import qualified Test.Cardano.Chain.Block.Validation
import qualified Test.Cardano.Chain.Common.Address
import qualified Test.Cardano.Chain.Common.Compact
import qualified Test.Cardano.Chain.Common.Lovelace
-- import qualified Test.Cardano.Chain.Block.Validation.Spec
import qualified Test.Cardano.Chain.Delegation.CBOR
import qualified Test.Cardano.Chain.Epoch.File
import qualified Test.Cardano.Chain.Genesis.Json
import qualified Test.Cardano.Chain.Slotting.Properties
import qualified Test.Cardano.Chain.Ssc.CBOR
import qualified Test.Cardano.Chain.Txp.CBOR
import qualified Test.Cardano.Chain.Txp.Compact
import qualified Test.Cardano.Chain.Txp.Json
import qualified Test.Cardano.Chain.Update.CBOR
import qualified Test.Cardano.Chain.Update.Json
import qualified Test.Cardano.Chain.Update.Properties
import qualified Test.Cardano.Chain.Elaboration.Delegation

main :: IO ()
main = do
  opts <- execParser optsParser
  let scenario = optsTestScenario opts
  putStrLn $ "\nRunning in scenario: " <> show scenario
  runTests
    [ Test.Cardano.Chain.Block.CBOR.tests scenario
    , Test.Cardano.Chain.Block.Validation.tests scenario
    , Test.Cardano.Chain.Common.Compact.tests scenario
    , Test.Cardano.Chain.Common.Lovelace.tests scenario
    , Test.Cardano.Chain.Common.Address.tests scenario
    , Test.Cardano.Chain.Delegation.CBOR.tests scenario
    , Test.Cardano.Chain.Epoch.File.tests
    , Test.Cardano.Chain.Genesis.Json.tests scenario
    , Test.Cardano.Chain.Elaboration.Delegation.tests scenario
    , Test.Cardano.Chain.Slotting.Properties.tests scenario
    , Test.Cardano.Chain.Ssc.CBOR.tests
    , Test.Cardano.Chain.Txp.CBOR.tests scenario
    , Test.Cardano.Chain.Txp.Compact.tests scenario
    , Test.Cardano.Chain.Txp.Json.tests scenario
    , Test.Cardano.Chain.Update.CBOR.tests scenario
    , Test.Cardano.Chain.Update.Json.tests scenario
    , Test.Cardano.Chain.Update.Properties.tests scenario
    -- , Test.Cardano.Chain.Block.Validation.Spec.tests scenario
    ]
