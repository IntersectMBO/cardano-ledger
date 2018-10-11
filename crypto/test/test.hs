import           Cardano.Prelude
import           Test.Cardano.Prelude

import           Test.Hspec (hspec)

import           Spec (spec)

import qualified Test.Cardano.Crypto.Bi

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Cardano.Crypto.Bi.tests
        ]
