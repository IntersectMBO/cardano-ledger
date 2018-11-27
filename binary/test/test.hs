import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Test.Cardano.Binary.Bi
import qualified Test.Cardano.Binary.BiSizeBounds


-- | Main testing action
main :: IO ()
main = runTests
  [Test.Cardano.Binary.Bi.tests, Test.Cardano.Binary.BiSizeBounds.tests]
