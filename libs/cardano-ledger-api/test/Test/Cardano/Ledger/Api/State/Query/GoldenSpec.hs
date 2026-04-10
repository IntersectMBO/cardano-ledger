{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.State.Query.GoldenSpec (spec) where

import Cardano.Ledger.Api.State.Query
import Cardano.Ledger.BaseTypes (EpochNo (..), unsafeNonZero)
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, Version, decodeFull)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (eraProtVerHigh)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley.API.Wallet (RewardInfoPool (..), RewardParams (..))
import Cardano.Ledger.State (ChainAccountState (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Test.Cardano.Ledger.Binary.Plain.Golden (DiffView (..), Enc (..), expectGoldenEncHexBytes)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Core.Utils (unsafeBoundRational)

-- | Pin all golden tests to Conway's highest protocol version
goldenVersion :: Version
goldenVersion = eraProtVerHigh @ConwayEra

-- | Bidirectional golden test: encoding produces expected hex, decoding expected hex
-- produces the value. This catches both encoder changes and decoder incompatibilities.
goldenRoundTrip ::
  forall a.
  (HasCallStack, EncCBOR a, DecCBOR a, Show a, Eq a) =>
  a ->
  BS.ByteString ->
  Spec
goldenRoundTrip val expectedHex = do
  it "encoding matches golden" $
    expectGoldenEncHexBytes DiffCBOR (Ev goldenVersion val) expectedHex
  it "decoding matches golden" $
    case BS16.decode expectedHex of
      Left err -> expectationFailure $ "Bad hex in test: " ++ err
      Right bs ->
        case decodeFull goldenVersion (BSL.fromStrict bs) of
          Left err -> expectationFailure $ "Decode failed: " ++ show err
          Right (decoded :: a) -> decoded `shouldBe` val

spec :: Spec
spec = describe "Query Golden" $ do
  describe "MemberStatus" $ do
    describe "Active" $ goldenRoundTrip Active "00"
    describe "Expired" $ goldenRoundTrip Expired "01"
    describe "Unrecognized" $ goldenRoundTrip Unrecognized "02"

  describe "HotCredAuthStatus" $ do
    describe "MemberAuthorized" $
      goldenRoundTrip
        (MemberAuthorized (KeyHashObj (mkKeyHash 0)))
        "82008200581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c0"
    describe "MemberNotAuthorized" $
      goldenRoundTrip MemberNotAuthorized "8101"
    describe "MemberResigned Nothing" $
      goldenRoundTrip (MemberResigned Nothing) "820280"

  describe "NextEpochChange" $ do
    describe "ToBeEnacted" $ goldenRoundTrip ToBeEnacted "8100"
    describe "ToBeRemoved" $ goldenRoundTrip ToBeRemoved "8101"
    describe "NoChangeExpected" $ goldenRoundTrip NoChangeExpected "8102"
    describe "ToBeExpired" $ goldenRoundTrip ToBeExpired "8103"
    describe "TermAdjusted" $ goldenRoundTrip (TermAdjusted (EpochNo 42)) "8204182a"

  describe "DefaultVote" $ do
    describe "DefaultNo" $ goldenRoundTrip DefaultNo "00"
    describe "DefaultAbstain" $ goldenRoundTrip DefaultAbstain "01"
    describe "DefaultNoConfidence" $ goldenRoundTrip DefaultNoConfidence "02"

  describe "CommitteeMemberState" $
    describe "canonical" $
      goldenRoundTrip
        (CommitteeMemberState MemberNotAuthorized Active (Just (EpochNo 100)) NoChangeExpected)
        "848101008118648102"

  describe "CommitteeMembersState" $
    describe "canonical" $
      goldenRoundTrip
        (CommitteeMembersState Map.empty Nothing (EpochNo 5))
        "83a08005"

  describe "RewardInfoPool" $
    describe "canonical" $
      goldenRoundTrip
        (RewardInfoPool (Coin 1000) (Coin 500) (Coin 300) (Coin 100) (unsafeBoundRational (1 % 2)) 1.5)
        "861903e81901f419012c1864d81e820102fb3ff8000000000000"

  describe "RewardParams" $
    describe "canonical" $
      goldenRoundTrip
        (RewardParams 500 (unsafeBoundRational (3 % 10)) (Coin 10000) (Coin 1000000))
        "841901f4d81e82030a1927101a000f4240"

  describe "ChainAccountState" $
    describe "canonical" $
      goldenRoundTrip
        (ChainAccountState (Coin 5000000) (Coin 3000000))
        "821a004c4b401a002dc6c0"

  describe "QueryPoolStateResult" $
    describe "canonical" $
      goldenRoundTrip
        (QueryPoolStateResult Map.empty Map.empty Map.empty Map.empty)
        "84a0a0a0a0"

  describe "StakeSnapshot" $
    describe "canonical" $
      goldenRoundTrip
        (StakeSnapshot (Coin 100) (Coin 200) (Coin 300))
        "83186418c819012c"

  describe "StakeSnapshots" $
    describe "canonical" $
      goldenRoundTrip
        ( StakeSnapshots
            Map.empty
            (unsafeNonZero (Coin 1000))
            (unsafeNonZero (Coin 2000))
            (unsafeNonZero (Coin 3000))
        )
        "84a01903e81907d0190bb8"
