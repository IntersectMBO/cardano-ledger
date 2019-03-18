import Cardano.Prelude
import Test.Cardano.Prelude

import System.Environment (withArgs)

import Test.Hspec (hspec)

import qualified Test.Cardano.Crypto.CryptoSpec
import qualified Test.Cardano.Crypto.CryptoSpec2

import qualified Test.Cardano.Crypto.Bi
import qualified Test.Cardano.Crypto.Json


-- | Main testing action
--
--   We use 'withArgs' to swallow common testing arguments that we want to parse
--   with `optparse-applicative`. This is only temporary until we remove
--   `hspec`, which is interfering.
main :: IO ()
main = withArgs [] $ do
  hspec $ do
    Test.Cardano.Crypto.CryptoSpec.spec
    Test.Cardano.Crypto.CryptoSpec2.spec

  runTests [Test.Cardano.Crypto.Bi.tests, Test.Cardano.Crypto.Json.tests]
