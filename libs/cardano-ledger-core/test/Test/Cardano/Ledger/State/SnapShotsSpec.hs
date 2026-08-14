{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.State.SnapShotsSpec (spec) where

import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing), Version)
import Cardano.Ledger.Binary (
  DecoderError (DecoderErrorDeserialiseFailure),
  decNoShareCBOR,
  encCBOR,
  natVersion,
 )
import Cardano.Ledger.State (LeiosKey, StakePoolSnapShot (..))
import Test.Cardano.Ledger.Binary.RoundTrip (
  RoundTripFailure (rtfDecoderError),
  Trip,
  embedTrip,
  embedTripExpectation,
  mkTrip,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

-- | 'StakePoolSnapShot' has no 'DecCBOR' instance, so the trip decodes without
-- sharing.
trip :: Trip StakePoolSnapShot StakePoolSnapShot
trip = mkTrip encCBOR decNoShareCBOR

-- | The generator leaves 'spssLeiosKey' empty, because a pool only registers a
-- BLS key from version 12 on. These properties set it, so they need a key of
-- their own.
withKey :: LeiosKey -> StakePoolSnapShot -> StakePoolSnapShot
withKey leiosKey spss = spss {spssLeiosKey = SJust leiosKey}

spec :: Spec
spec =
  describe "StakePoolSnapShot" $ do
    prop "keeps spssLeiosKey at version 12" $ \leiosKey spss ->
      embedTripExpectation (natVersion @12) (natVersion @12) trip shouldBe $
        withKey leiosKey spss

    prop "leaves spssLeiosKey out before version 12" $ \leiosKey spss ->
      embedTripExpectation
        (natVersion @11)
        (natVersion @11)
        trip
        (\decoded original -> decoded `shouldBe` original {spssLeiosKey = SNothing})
        $ withKey leiosKey spss

    -- The version decides the field count on both sides, so a record written at
    -- one version does not decode at the other.
    prop "rejects a ten field encoding at version 12" $ \leiosKey spss ->
      expectDecodeFailure (natVersion @11) (natVersion @12) (withKey leiosKey spss)

    prop "rejects an eleven field encoding before version 12" $ \leiosKey spss ->
      expectDecodeFailure (natVersion @12) (natVersion @11) (withKey leiosKey spss)

-- | The wrong field count misaligns the record, so the decode fails on the field
-- that follows 'spssLeiosKey' rather than on the size check. Name the record, so
-- that a failure somewhere else does not pass for one here.
expectDecodeFailure :: Version -> Version -> StakePoolSnapShot -> Expectation
expectDecodeFailure encVersion decVersion spss =
  case embedTrip encVersion decVersion trip spss of
    Right decoded -> expectationFailure $ "Expected a failure, decoded: " <> show decoded
    Left failure -> case rtfDecoderError failure of
      Just (DecoderErrorDeserialiseFailure recordName _) ->
        recordName `shouldBe` "StakePoolSnapShot"
      other -> expectationFailure $ "Expected a decode failure, got: " <> show other
