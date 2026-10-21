{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.TxInfoSpec (
  txInfoSpec,
  validBetweenEpochInfo,
  testEpochInfo,
  testSystemStart,
) where

import Cardano.Ledger.Alonzo.Plutus.TxInfo (AlonzoContextError (..), transValidityInterval)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (EpochInfo (..), fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Either (isRight)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (genOrderedPair)

txInfoSpec :: forall era. AlonzoEraImp era => Spec
txInfoSpec = describe "Alonzo TxInfoSpec" $ do
  describe "Translation" $ do
    describe "transValidityInterval" $ do
      prop "translates successfully if bounds are valid" $ do
        (hi, lo) <- genOrderedPair
        validityInterval <- genGoodValidityInterval lo hi
        pure $
          transValidityInterval
            (Proxy @era)
            (validBetweenEpochInfo lo hi)
            testSystemStart
            validityInterval
            `shouldSatisfy` isRight
      prop "fails to translate if the bounds are invalid" $ do
        (hi, lo) <- genOrderedPair
        validityInterval <- genBadValidityInterval lo hi
        pure $
          transValidityInterval
            (Proxy @era)
            (validBetweenEpochInfo lo hi)
            testSystemStart
            validityInterval
            `shouldBeLeft` inject (TimeTranslationPastHorizon @era "out of valid range")

validBetweenEpochInfo :: SlotNo -> SlotNo -> EpochInfo (Either Text)
validBetweenEpochInfo start end =
  testEpochInfo
    { epochInfoSlotToRelativeTime_ = \slot ->
        if slot > end || slot < start
          then Left "out of valid range"
          else epochInfoSlotToRelativeTime_ testEpochInfo slot
    }

testEpochInfo :: EpochInfo (Either a)
testEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

testSystemStart :: SystemStart
testSystemStart = SystemStart $ posixSecondsToUTCTime 0
