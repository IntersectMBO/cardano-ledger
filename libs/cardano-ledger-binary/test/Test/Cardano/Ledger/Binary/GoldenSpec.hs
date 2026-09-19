module Test.Cardano.Ledger.Binary.GoldenSpec (spec) where

import Cardano.Crypto.Leios (LeiosCert)
import Paths_cardano_ledger_binary (getDataFileName)
import Test.Cardano.Crypto.Leios.Gen (genLeiosCert, generateWith)
import Test.Cardano.Ledger.Binary.Golden (cborGoldenSpec)
import Test.Hspec (Spec, describe)

spec :: Spec
spec =
  describe "Golden" $
    cborGoldenSpec getDataFileName "golden/LeiosCert.cbor" maxBound exampleCert

exampleCert :: LeiosCert
exampleCert = genLeiosCert `generateWith` (42 :: Int)
