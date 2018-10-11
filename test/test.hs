import           Cardano.Prelude
import           Test.Cardano.Prelude

import qualified Test.Cardano.Chain.Block.Bi
import qualified Test.Cardano.Chain.Delegation.Bi
import qualified Test.Cardano.Chain.Ssc.Bi
import qualified Test.Cardano.Chain.Txp.Bi
import qualified Test.Cardano.Chain.Update.Bi
import qualified Test.Cardano.Chain.Update.Json

main :: IO ()
main = runTests
    [ Test.Cardano.Chain.Block.Bi.tests
    , Test.Cardano.Chain.Delegation.Bi.tests
    , Test.Cardano.Chain.Ssc.Bi.tests
    , Test.Cardano.Chain.Txp.Bi.tests
    , Test.Cardano.Chain.Update.Bi.tests
    , Test.Cardano.Chain.Update.Json.tests
    ]
