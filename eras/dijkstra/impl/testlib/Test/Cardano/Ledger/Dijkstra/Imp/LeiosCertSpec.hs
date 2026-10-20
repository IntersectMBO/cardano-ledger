{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LeiosCertSpec (dijkstraOnlySpec) where

import Cardano.Crypto.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Shelley.LedgerState
import Data.List.NonEmpty (NonEmpty)
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

-- | Three keyed seats reach the quorum, the fourth seat has stake but no key.
testCommittee :: ([LeiosSigningKey], LeiosCommittee)
testCommittee = mkCommitteeFromTestSeats $ replicate 3 (WithKey, 1 / 4) <> [(NoKey, 1 / 4)]

-- | A certificate from three seats for an announcement in the current slot, which can be
-- included 14 slots later.
certifiedAnnouncement :: ImpTestM DijkstraEra (LeiosCertEnv, LeiosCert)
certifiedAnnouncement = do
  headerHash <- arbitrary
  annSlot <- getCurSlotNo
  let (keys, committee) = testCommittee
  cert <- mkLeiosCert committee keys [0, 1, 2] headerHash
  pure
    ( LeiosCertEnv
        { lseCurrentSlotNo = annSlot + 14
        , lseAnnouncementSlotNo = annSlot
        , lseAnnouncedInBlockHeader = headerHash
        , lseLeiosCommittee = committee
        , lseThreshold = 3 %! 4
        , lseAnnouncementPeriodLength = Milliseconds32 1_000
        , lseVotePeriodLength = Milliseconds32 4_000
        , lseDiffusionPeriodLength = Milliseconds32 7_000
        }
    , cert
    )

runLeiosCert ::
  LeiosCertEnv ->
  LeiosCert ->
  ImpTestM DijkstraEra (Either (NonEmpty (DijkstraLeiosCertPredFailure DijkstraEra)) ())
runLeiosCert env cert = fmap fst <$> tryRunImpRule @"LEIOSCERT" env () cert

dijkstraOnlySpec :: SpecWith (ImpInit (LedgerSpec DijkstraEra))
dijkstraOnlySpec = describe "LEIOSCERT" $ do
  it "LeiosCertTooEarly" $ do
    (env, cert) <- certifiedAnnouncement
    let earliestSlot = lseCurrentSlotNo env
    runLeiosCert env {lseCurrentSlotNo = earliestSlot - 1} cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch (earliestSlot - 1) earliestSlot)]
    runLeiosCert env cert >>= expectRightDeep_

  it "Rounds the certification delay up to whole slots" $ do
    (env, cert) <- certifiedAnnouncement
    let annSlot = lseAnnouncementSlotNo env
        env' = env {lseDiffusionPeriodLength = Milliseconds32 7_001}
    runLeiosCert env' {lseCurrentSlotNo = annSlot + 14} cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch (annSlot + 14) (annSlot + 15))]
    runLeiosCert env' {lseCurrentSlotNo = annSlot + 15} cert >>= expectRightDeep_

  it "LeiosCertAnnouncedInPreviousEpoch" $ do
    (env, cert) <- certifiedAnnouncement
    annEpoch <- getsNES nesELL
    passEpoch
    curEpoch <- getsNES nesELL
    curSlot <- getCurSlotNo
    runLeiosCert env {lseCurrentSlotNo = curSlot} cert
      `shouldReturn` Left [LeiosCertAnnouncedInPreviousEpoch (Mismatch annEpoch curEpoch)]

  it "LeiosCertInsufficientWeight" $ do
    (env, _) <- certifiedAnnouncement
    let (keys, committee) = testCommittee
    cert <- mkLeiosCert committee keys [0, 1] $ lseAnnouncedInBlockHeader env
    runLeiosCert env cert
      `shouldReturn` Left [LeiosCertInsufficientWeight (Mismatch (1 / 2) (3 / 4))]

  it "LeiosCertInvalidSignature" $ do
    (env, cert) <- certifiedAnnouncement
    otherHeaderHash <- arbitrary
    runLeiosCert env {lseAnnouncedInBlockHeader = otherHeaderHash} cert
      `shouldReturn` Left [LeiosCertInvalidSignature]

  it "LeiosCertSignersWithoutKey" $ do
    (env, _) <- certifiedAnnouncement
    let (keys, committee) = testCommittee
    cert <- mkLeiosCert committee keys [0, 1, 2, 3] $ lseAnnouncedInBlockHeader env
    runLeiosCert env cert `shouldReturn` Left [LeiosCertSignersWithoutKey [3]]
