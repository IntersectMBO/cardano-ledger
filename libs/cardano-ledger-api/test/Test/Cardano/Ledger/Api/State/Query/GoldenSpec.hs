{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.State.Query.GoldenSpec (spec) where

import Cardano.Ledger.Address (AccountAddress (..), AccountId (..))
import Cardano.Ledger.Api.State.Query
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  EpochNo (..),
  Network (..),
  StrictMaybe (..),
  textToUrl,
  unsafeNonZero,
 )
import Cardano.Ledger.Binary (DecCBOR, EncCBOR, Version, decodeFull)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (eraProtVerHigh)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Hashes (ADDRHASH, HASH, ScriptHash (..), VRFVerKeyHash (..))
import Cardano.Ledger.Shelley.API.Wallet (RewardInfoPool (..), RewardParams (..))
import Cardano.Ledger.State (ChainAccountState (..), StakePoolParams (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Binary.Plain.Golden (DiffView (..), Enc (..), expectGoldenEncHexBytes)
import Test.Cardano.Ledger.Binary.Random (mkDummyHash)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.KeyPair (mkKeyHash)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash, unsafeBoundRational)

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
    describe "MemberResigned (Just anchor)" $
      goldenRoundTrip
        (MemberResigned (Just (Anchor (fromJust $ textToUrl 64 "https://example.com") (mkDummySafeHash 0))))
        "820281827368747470733a2f2f6578616d706c652e636f6d582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"

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

  describe "QueryResultCommitteeMemberState" $ do
    describe "canonical" $
      goldenRoundTrip
        (QueryResultCommitteeMemberState MemberNotAuthorized Active (Just (EpochNo 100)) NoChangeExpected)
        "848101008118648102"
    describe "Nothing expiration" $
      goldenRoundTrip
        (QueryResultCommitteeMemberState MemberNotAuthorized Unrecognized Nothing ToBeRemoved)
        "84810102808101"

  describe "QueryResultCommitteeMembersState" $ do
    describe "canonical" $
      goldenRoundTrip
        (QueryResultCommitteeMembersState Map.empty Nothing (EpochNo 5))
        "83a08005"
    describe "Just threshold" $
      goldenRoundTrip
        (QueryResultCommitteeMembersState Map.empty (Just (unsafeBoundRational (2 % 3))) (EpochNo 10))
        "83a081d81e8202030a"
    describe "non-empty committee" $
      goldenRoundTrip
        ( QueryResultCommitteeMembersState
            ( Map.singleton
                (KeyHashObj (mkKeyHash 0))
                (QueryResultCommitteeMemberState MemberNotAuthorized Active (Just (EpochNo 100)) NoChangeExpected)
            )
            Nothing
            (EpochNo 5)
        )
        "83a18200581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c08481010081186481028005"

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

  describe "QueryPoolStateResult" $ do
    describe "canonical" $
      goldenRoundTrip
        (QueryPoolStateResult Map.empty Map.empty Map.empty Map.empty)
        "84a0a0a0a0"
    describe "non-empty pools" $
      let poolId = mkKeyHash 0
          sampleParams =
            StakePoolParams
              { sppId = poolId
              , sppVrf = VRFVerKeyHash (mkDummyHash @HASH (0 :: Int))
              , sppPledge = Coin 1000
              , sppCost = Coin 500
              , sppMargin = unsafeBoundRational (1 % 10)
              , sppAccountAddress = AccountAddress Testnet (AccountId (KeyHashObj (mkKeyHash 1)))
              , sppOwners = Set.singleton (mkKeyHash 2)
              , sppRelays = mempty
              , sppMetadata = SNothing
              }
       in goldenRoundTrip
            ( QueryPoolStateResult
                (Map.singleton poolId sampleParams)
                Map.empty
                (Map.singleton poolId (EpochNo 100))
                (Map.singleton poolId (Coin 500))
            )
            "84a1581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c089581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c0582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c1113141903e81901f4d81e82010a581de0903884936fec86ccd3e6133e289e7f950b709a60bf64af0f9dc64410d9010281581c8a209f61d4a1a3fa94418e4a402880b758cf52552f13fe212a94b43680f6a0a1581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c01864a1581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c01901f4"

  describe "QueryResultDelegsAndRewards" $ do
    describe "canonical" $
      goldenRoundTrip
        (QueryResultDelegsAndRewards Map.empty Map.empty)
        "82a0a0"
    describe "non-empty delegations and rewards" $
      goldenRoundTrip
        ( QueryResultDelegsAndRewards
            (Map.singleton (KeyHashObj (mkKeyHash 0)) (mkKeyHash 1))
            (Map.singleton (KeyHashObj (mkKeyHash 0)) (Coin 1000))
        )
        "82a18200581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c0581c903884936fec86ccd3e6133e289e7f950b709a60bf64af0f9dc64410a18200581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c01903e8"

  describe "QueryResultStakeSnapshot" $
    describe "canonical" $
      goldenRoundTrip
        (QueryResultStakeSnapshot (Coin 100) (Coin 200) (Coin 300))
        "83186418c819012c"

  describe "QueryResultStakeSnapshots" $ do
    describe "canonical" $
      goldenRoundTrip
        ( QueryResultStakeSnapshots
            Map.empty
            (unsafeNonZero (Coin 1000))
            (unsafeNonZero (Coin 2000))
            (unsafeNonZero (Coin 3000))
        )
        "84a01903e81907d0190bb8"
    describe "non-empty snapshots" $
      goldenRoundTrip
        ( QueryResultStakeSnapshots
            (Map.singleton (mkKeyHash 0) (QueryResultStakeSnapshot (Coin 100) (Coin 200) (Coin 300)))
            (unsafeNonZero (Coin 1000))
            (unsafeNonZero (Coin 2000))
            (unsafeNonZero (Coin 3000))
        )
        "84a1581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c083186418c819012c1903e81907d0190bb8"

  describe "QueryResultConstitution" $ do
    describe "canonical" $
      goldenRoundTrip
        ( QueryResultConstitution
            (Anchor (fromJust $ textToUrl 64 "https://example.com") (mkDummySafeHash 0))
            Nothing
        )
        "82827368747470733a2f2f6578616d706c652e636f6d582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c11131480"
    describe "Just guardrailsScript" $
      goldenRoundTrip
        ( QueryResultConstitution
            (Anchor (fromJust $ textToUrl 64 "https://example.com") (mkDummySafeHash 0))
            (Just (ScriptHash (mkDummyHash @ADDRHASH (1 :: Int))))
        )
        "82827368747470733a2f2f6578616d706c652e636f6d582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c11131481581ce0a714319812c3f773ba04ec5d6b3ffcd5aad85006805b047b082541"

  describe "QueryResultDRepState" $ do
    describe "canonical" $
      goldenRoundTrip
        (QueryResultDRepState (EpochNo 100) Nothing (Coin 500) Set.empty)
        "841864801901f4d9010280"
    describe "Just anchor" $
      goldenRoundTrip
        ( QueryResultDRepState
            (EpochNo 100)
            (Just (Anchor (fromJust $ textToUrl 64 "https://example.com") (mkDummySafeHash 0)))
            (Coin 500)
            (Set.singleton (KeyHashObj (mkKeyHash 0)))
        )
        "84186481827368747470733a2f2f6578616d706c652e636f6d582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c1113141901f4d90102818200581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c0"

  describe "QueryResultDRepStates" $ do
    describe "canonical" $
      goldenRoundTrip
        (QueryResultDRepStates Map.empty)
        "a0"
    describe "non-empty states" $
      goldenRoundTrip
        ( QueryResultDRepStates
            ( Map.singleton
                (KeyHashObj (mkKeyHash 0))
                ( QueryResultDRepState
                    (EpochNo 100)
                    (Just (Anchor (fromJust $ textToUrl 64 "https://example.com") (mkDummySafeHash 0)))
                    (Coin 500)
                    mempty
                )
            )
        )
        "a18200581c73dedba5efd33678f941c6ee48cdf2b35ff4f40653e9ed01d6c5e3c084186481827368747470733a2f2f6578616d706c652e636f6d582003170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c1113141901f4d9010280"
