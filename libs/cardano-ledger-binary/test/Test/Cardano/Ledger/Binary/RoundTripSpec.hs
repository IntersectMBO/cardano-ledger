{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.RoundTripSpec (spec) where

import Cardano.Ledger.Binary
import Control.Monad (forM_)
import Data.Int
import Data.Word
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Hspec
import Data.Time.Clock (NominalDiffTime, UTCTime (..))
import Data.Fixed (Fixed (..), Nano, Pico)



spec :: Spec
spec =
  describe "RoundTrip" $ do
    forM_ allVersions $ \version ->
      describe (show version) $ do
        roundTripSpec @() version cborTrip
        roundTripSpec @Bool version cborTrip
        roundTripSpec @Integer version cborTrip
        roundTripSpec @Word version cborTrip
        roundTripSpec @Word8 version cborTrip
        roundTripSpec @Word16 version cborTrip
        roundTripSpec @Word32 version cborTrip
        roundTripSpec @Word64 version cborTrip
        roundTripSpec @Int version cborTrip
        roundTripSpec @Int8 version cborTrip
        roundTripSpec @Int16 version cborTrip
        roundTripSpec @Int32 version cborTrip
        roundTripSpec @Int64 version cborTrip
        roundTripSpec @Float version cborTrip
        roundTripSpec @Double version cborTrip
        roundTripSpec @Rational version cborTrip
        roundTripSpec @Nano version cborTrip
        roundTripSpec @Pico version cborTrip
