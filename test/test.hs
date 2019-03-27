module Main
  ( main
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Options.Applicative (execParser)

import Test.Options (Opts(..), optsParser)

import qualified Test.Cardano.Chain.Block.Bi
import qualified Test.Cardano.Chain.Block.Validation
import qualified Test.Cardano.Chain.Common.Address
import qualified Test.Cardano.Chain.Common.Lovelace
import qualified Test.Cardano.Chain.Block.Validation.Spec
import qualified Test.Cardano.Chain.Delegation.Bi
import qualified Test.Cardano.Chain.Epoch.File
import qualified Test.Cardano.Chain.Genesis.Json
import qualified Test.Cardano.Chain.Slotting.Properties
import qualified Test.Cardano.Chain.Ssc.Bi
import qualified Test.Cardano.Chain.Txp.Bi
import qualified Test.Cardano.Chain.Txp.Json
import qualified Test.Cardano.Chain.Txp.Validation
import qualified Test.Cardano.Chain.Update.Bi
import qualified Test.Cardano.Chain.Update.Json
import qualified Test.Cardano.Chain.Update.Properties
import qualified Test.Cardano.Chain.Elaboration.Delegation

main :: IO ()
main = do
  opts <- execParser optsParser
  let scenario = optsTestScenario opts
  runTests
    [ Test.Cardano.Chain.Block.Bi.tests
    , Test.Cardano.Chain.Block.Validation.tests scenario
    , Test.Cardano.Chain.Common.Lovelace.tests
    , Test.Cardano.Chain.Common.Address.tests
    , Test.Cardano.Chain.Delegation.Bi.tests
    , Test.Cardano.Chain.Epoch.File.tests
    , Test.Cardano.Chain.Genesis.Json.tests
    , Test.Cardano.Chain.Elaboration.Delegation.tests
    , Test.Cardano.Chain.Slotting.Properties.tests
    , Test.Cardano.Chain.Ssc.Bi.tests
    , Test.Cardano.Chain.Txp.Bi.tests
    , Test.Cardano.Chain.Txp.Json.tests
    , Test.Cardano.Chain.Txp.Validation.tests scenario
    , Test.Cardano.Chain.Update.Bi.tests
    , Test.Cardano.Chain.Update.Json.tests
    , Test.Cardano.Chain.Update.Properties.tests
    , Test.Cardano.Chain.Block.Validation.Spec.tests
    ]
