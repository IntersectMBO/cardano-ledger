import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Cardano.Binary.BiSizeBounds

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Cardano.Binary.BiSizeBounds.tests
        ]
