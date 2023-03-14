module Main where

import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Ledger.Binary.Failure as Failure
import qualified Test.Cardano.Ledger.Binary.PlainSpec as PlainSpec
import qualified Test.Cardano.Ledger.Binary.RoundTripSpec as RoundTripSpec
import qualified Test.Cardano.Ledger.Binary.Success as Success
import qualified Test.Cardano.Ledger.Binary.Vintage.Coders as Vintage.Coders
import qualified Test.Cardano.Ledger.Binary.Vintage.Drop as Vintage.Drop
import qualified Test.Cardano.Ledger.Binary.Vintage.Failure as Vintage.Failure
import qualified Test.Cardano.Ledger.Binary.Vintage.RoundTrip as Vintage.RoundTrip
import qualified Test.Cardano.Ledger.Binary.Vintage.Serialization as Vintage.Serialization
import qualified Test.Cardano.Ledger.Binary.Vintage.SizeBounds as Vintage.SizeBounds
import Test.Hspec

spec :: Spec
spec = do
  describe "Vintage Test Suite" $ do
    it "RoundTrip" $ Vintage.RoundTrip.tests `shouldReturn` True
    it "SizeBounds" $ Vintage.SizeBounds.tests `shouldReturn` True
    it "Serialization" $ Vintage.Serialization.tests `shouldReturn` True
    it "Drop" $ Vintage.Drop.tests `shouldReturn` True
    it "Failure" $ Vintage.Failure.tests `shouldReturn` True
    Vintage.Coders.spec
  PlainSpec.spec
  describe "Versioned" $ do
    RoundTripSpec.spec
    Failure.spec
    Success.spec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hspec spec
