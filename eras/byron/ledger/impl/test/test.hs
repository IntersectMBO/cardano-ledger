module Main
  ( main,
  )
where

import Cardano.Prelude
import qualified Test.Cardano.Chain.Block.CBOR
import qualified Test.Cardano.Chain.Block.Model
import qualified Test.Cardano.Chain.Block.Size
import qualified Test.Cardano.Chain.Block.Validation
import qualified Test.Cardano.Chain.Block.ValidationMode
import qualified Test.Cardano.Chain.Buildable
import qualified Test.Cardano.Chain.Byron.API
import qualified Test.Cardano.Chain.Common.Address
import qualified Test.Cardano.Chain.Common.Attributes
import qualified Test.Cardano.Chain.Common.CBOR
import qualified Test.Cardano.Chain.Common.Compact
import qualified Test.Cardano.Chain.Common.Lovelace
import qualified Test.Cardano.Chain.Delegation.CBOR
import qualified Test.Cardano.Chain.Delegation.Certificate
import qualified Test.Cardano.Chain.Delegation.Model
import qualified Test.Cardano.Chain.Elaboration.Delegation
import qualified Test.Cardano.Chain.Epoch.File
import qualified Test.Cardano.Chain.Genesis.CBOR
import qualified Test.Cardano.Chain.Genesis.Json
import qualified Test.Cardano.Chain.MempoolPayload.CBOR
import qualified Test.Cardano.Chain.Slotting.CBOR
import qualified Test.Cardano.Chain.Slotting.Properties
import qualified Test.Cardano.Chain.Ssc.CBOR
import qualified Test.Cardano.Chain.UTxO.CBOR
import qualified Test.Cardano.Chain.UTxO.Compact
import qualified Test.Cardano.Chain.UTxO.Model
import qualified Test.Cardano.Chain.UTxO.ValidationMode
import qualified Test.Cardano.Chain.Update.CBOR
import qualified Test.Cardano.Chain.Update.Properties
import Test.Options (ShouldAssertNF (..), mainWithTestScenario, tsGroupToTree)
import Test.Tasty (testGroup)

main :: IO ()
main =
  mainWithTestScenario $
    testGroup "Cardano Ledger Tests" $
      tsGroupToTree
        <$> [ Test.Cardano.Chain.Block.CBOR.tests,
              Test.Cardano.Chain.Block.Model.tests,
              Test.Cardano.Chain.Block.Size.tests,
              Test.Cardano.Chain.Block.Validation.tests NoAssertNF,
              Test.Cardano.Chain.Block.ValidationMode.tests,
              Test.Cardano.Chain.Buildable.tests,
              Test.Cardano.Chain.Common.Address.tests,
              Test.Cardano.Chain.Common.Attributes.tests,
              Test.Cardano.Chain.Common.CBOR.tests,
              Test.Cardano.Chain.Common.Compact.tests,
              Test.Cardano.Chain.Common.Lovelace.tests,
              Test.Cardano.Chain.Delegation.CBOR.tests,
              const Test.Cardano.Chain.Delegation.Certificate.tests,
              const Test.Cardano.Chain.Delegation.Model.tests,
              const Test.Cardano.Chain.Epoch.File.tests,
              Test.Cardano.Chain.Elaboration.Delegation.tests,
              Test.Cardano.Chain.Genesis.CBOR.tests,
              Test.Cardano.Chain.Genesis.Json.tests,
              Test.Cardano.Chain.MempoolPayload.CBOR.tests,
              Test.Cardano.Chain.Slotting.CBOR.tests,
              Test.Cardano.Chain.Slotting.Properties.tests,
              const Test.Cardano.Chain.Ssc.CBOR.tests,
              Test.Cardano.Chain.UTxO.CBOR.tests,
              Test.Cardano.Chain.UTxO.Compact.tests,
              Test.Cardano.Chain.UTxO.Model.tests,
              Test.Cardano.Chain.UTxO.ValidationMode.tests,
              Test.Cardano.Chain.Update.CBOR.tests,
              Test.Cardano.Chain.Update.Properties.tests,
              Test.Cardano.Chain.Byron.API.tests
            ]
