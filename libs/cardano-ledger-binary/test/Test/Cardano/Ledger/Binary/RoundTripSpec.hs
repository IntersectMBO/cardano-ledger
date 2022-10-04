{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.RoundTripSpec (spec) where

import Cardano.Ledger.Binary
import Control.Monad (forM_)
import Data.Fixed (Fixed (..), Nano, Pico)
import Data.GenValidity
import Data.GenValidity.Time.Clock ()
import Data.Int
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime (..),
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import Data.Word
import Numeric.Natural
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Hspec

-- | We do not handle the full precision of NominalDiffTime.
newtype NominalDiffTimeRounded = NominalDiffTimeRounded NominalDiffTime
  deriving (Show, Eq, ToCBOR, FromCBOR, Validity)

instance GenValid NominalDiffTimeRounded where
  genValid = secondsToNominalDiffTimeRounded <$> genValid
  shrinkValid = fmap secondsToNominalDiffTimeRounded . shrinkValid . nominalDiffTimeRoundedToSeconds

secondsToNominalDiffTimeRounded :: Pico -> NominalDiffTimeRounded
secondsToNominalDiffTimeRounded (MkFixed s) =
  NominalDiffTimeRounded $
    secondsToNominalDiffTime $ MkFixed (1_000_000 * (s `div` 1_000_000))

nominalDiffTimeRoundedToSeconds :: NominalDiffTimeRounded -> Pico
nominalDiffTimeRoundedToSeconds (NominalDiffTimeRounded ndt) = nominalDiffTimeToSeconds ndt

spec :: Spec
spec =
  describe "RoundTrip" $ do
    forM_ allVersions $ \version ->
      describe (show version) $ do
        roundTripSpec @() version cborTrip
        roundTripSpec @Bool version cborTrip
        roundTripValidSpec @Integer version cborTrip
        roundTripValidSpec @Natural version cborTrip
        roundTripValidSpec @Word version cborTrip
        roundTripValidSpec @Word8 version cborTrip
        roundTripValidSpec @Word16 version cborTrip
        roundTripValidSpec @Word32 version cborTrip
        roundTripValidSpec @Word64 version cborTrip
        roundTripValidSpec @Int version cborTrip
        roundTripValidSpec @Int8 version cborTrip
        roundTripValidSpec @Int16 version cborTrip
        roundTripValidSpec @Int32 version cborTrip
        roundTripValidSpec @Int64 version cborTrip
        roundTripSpec @Float version cborTrip
        roundTripSpec @Double version cborTrip
        roundTripValidSpec @Rational version cborTrip
        roundTripValidSpec @Nano version cborTrip
        roundTripValidSpec @Pico version cborTrip
        roundTripValidSpec @NominalDiffTimeRounded version cborTrip
        roundTripValidSpec @UTCTime version cborTrip
