module Main where

import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Ledger.Binary.Vintage.Coders
import qualified Test.Cardano.Ledger.Binary.Vintage.Drop
import qualified Test.Cardano.Ledger.Binary.Vintage.Failure
import qualified Test.Cardano.Ledger.Binary.Vintage.RoundTrip
import qualified Test.Cardano.Ledger.Binary.Vintage.Serialization
import qualified Test.Cardano.Ledger.Binary.Vintage.SizeBounds
import Test.Hspec

spec :: Spec
spec = do
  describe "Vintage Test Suite" $ do
    it "RoundTrip" $ Test.Cardano.Ledger.Binary.Vintage.RoundTrip.tests `shouldReturn` True
    it "SizeBounds" $ Test.Cardano.Ledger.Binary.Vintage.SizeBounds.tests `shouldReturn` True
    it "Serialization" $ Test.Cardano.Ledger.Binary.Vintage.Serialization.tests `shouldReturn` True
    it "Drop" $ Test.Cardano.Ledger.Binary.Vintage.Drop.tests `shouldReturn` True
    it "Failure" $ Test.Cardano.Ledger.Binary.Vintage.Failure.tests `shouldReturn` True
    Test.Cardano.Ledger.Binary.Vintage.Coders.spec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspec spec
