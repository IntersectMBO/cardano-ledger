import Cardano.Prelude
import Test.Cardano.Prelude

import qualified Test.Cardano.Crypto.CBOR
import qualified Test.Cardano.Crypto.Hashing
import qualified Test.Cardano.Crypto.HD
import qualified Test.Cardano.Crypto.Json
import qualified Test.Cardano.Crypto.Limits
import qualified Test.Cardano.Crypto.Keys
import qualified Test.Cardano.Crypto.Random
import qualified Test.Cardano.Crypto.Signing.Proxy
import qualified Test.Cardano.Crypto.Signing.Redeem
import qualified Test.Cardano.Crypto.Signing.Safe
import qualified Test.Cardano.Crypto.Signing.Signing


-- | Main testing action
main :: IO ()
main = runTests
  [ Test.Cardano.Crypto.CBOR.tests
  , Test.Cardano.Crypto.Hashing.tests
  , Test.Cardano.Crypto.HD.tests
  , Test.Cardano.Crypto.Json.tests
  , Test.Cardano.Crypto.Keys.tests
  , Test.Cardano.Crypto.Limits.tests
  , Test.Cardano.Crypto.Random.tests
  , Test.Cardano.Crypto.Signing.Proxy.tests
  , Test.Cardano.Crypto.Signing.Redeem.tests
  , Test.Cardano.Crypto.Signing.Safe.tests
  , Test.Cardano.Crypto.Signing.Signing.tests
  ]
