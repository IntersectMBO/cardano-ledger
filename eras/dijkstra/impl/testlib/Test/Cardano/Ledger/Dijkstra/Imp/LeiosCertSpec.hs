{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LeiosCertSpec (dijkstraOnlySpec) where

import Cardano.Crypto.DSIGN (signDSIGN)
import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Crypto.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Hashes (extractHash)
import Cardano.Ledger.Shelley.LedgerState (nesELL)
import Cardano.Ledger.Slot (epochInfoFirst)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word32)
import Lens.Micro.Mtl (use)
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Cardano.Ledger.Core.Rational ((%!))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

-- Three keyed seats reach quorum; the fourth seat has stake but no BLS key.
testCommittee :: ([LeiosSigningKey], LeiosCommittee)
testCommittee = mkCommitteeFromTestSeats $ replicate 3 (WithKey, 1 / 4) <> [(NoKey, 1 / 4)]

makeCert ::
  LeiosCommittee ->
  [LeiosSigningKey] ->
  [Word16] ->
  BS.ByteString ->
  ImpTestM era LeiosCert
makeCert committee keys signers msg =
  expectRightDeep $
    aggregateLeiosCert committee $
      Map.fromList
        [ (LeiosSeatId i, signDSIGN leiosSignContext msg (keys !! fromIntegral i))
        | i <- signers
        ]

fixture :: ImpTestM DijkstraEra (LeiosCertEnv, LeiosCert)
fixture = do
  headerHash <- arbitrary
  annSlot <- getCurSlotNo
  let (keys, committee) = testCommittee
      env =
        LeiosCertEnv
          { lseCurrentSlotNo = annSlot + 14
          , lseAnnouncementSlotNo = annSlot
          , lseAnnouncedInBlockHeader = headerHash
          , lseLeiosCommittee = committee
          , lseThreshold = 3 %! 4
          , lseAnnouncementPeriodLength = Milliseconds32 1_000
          , lseVotePeriodLength = Milliseconds32 4_000
          , lseDiffusionPeriodLength = Milliseconds32 7_000
          }
  cert <- makeCert committee keys [0, 1, 2] (leiosCertMessage headerHash)
  pure (env, cert)

dijkstraOnlySpec :: SpecWith (ImpInit (LedgerSpec DijkstraEra))
dijkstraOnlySpec = describe "LEIOSCERT" $ do
  it "Accepts a certificate at exactly the earliest slot" $ do
    (env, cert) <- fixture
    tryRunImpRule @"LEIOSCERT" env () cert `shouldReturn` Right ((), [])

  it "Rejects a certificate one slot before the earliest slot" $ do
    (env, cert) <- fixture
    let cur = lseCurrentSlotNo env - 1
    tryRunImpRule @"LEIOSCERT" env {lseCurrentSlotNo = cur} () cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch cur (lseCurrentSlotNo env))]

  describe "Rounds the certification delay up to slots" $
    forM_ ([(750, 1), (1_000, 1), (1_001, 2), (0, 0)] :: [(Word32, SlotNo)]) $ \(delayMs, delaySlots) ->
      it (show delayMs <> " milliseconds") $ do
        (env, cert) <- fixture
        let annSlot = lseAnnouncementSlotNo env
            earliest = annSlot + delaySlots
            env' =
              env
                { lseCurrentSlotNo = earliest
                , lseAnnouncementPeriodLength = Milliseconds32 0
                , lseVotePeriodLength = Milliseconds32 delayMs
                , lseDiffusionPeriodLength = Milliseconds32 0
                }
        tryRunImpRule @"LEIOSCERT" env' () cert `shouldReturn` Right ((), [])
        if delaySlots == 0
          then pure ()
          else
            tryRunImpRule @"LEIOSCERT" env' {lseCurrentSlotNo = earliest - 1} () cert
              `shouldReturn` Left [LeiosCertTooEarly (Mismatch (earliest - 1) earliest)]

  it "Does not overflow when combining maximum period lengths" $ do
    (env, cert) <- fixture
    let annSlot = lseAnnouncementSlotNo env
        env' =
          env
            { lseCurrentSlotNo = annSlot
            , lseAnnouncementPeriodLength = maxBound
            , lseVotePeriodLength = maxBound
            , lseDiffusionPeriodLength = maxBound
            }
    tryRunImpRule @"LEIOSCERT" env' () cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch annSlot (annSlot + 21_474_837))]

  it "Rejects an announcement from the previous epoch" $ do
    (env, cert) <- fixture
    epoch <- getsNES nesELL
    globals <- use impGlobalsL
    let nextEpochSlot = epochInfoFirst (epochInfoPure globals) (epoch + 1)
    tryRunImpRule @"LEIOSCERT" env {lseCurrentSlotNo = nextEpochSlot} () cert
      `shouldReturn` Left [LeiosCertAnnouncedInPreviousEpoch (Mismatch epoch (epoch + 1))]

  it "Rejects insufficient stake weight" $ do
    (env, _) <- fixture
    let (keys, committee) = testCommittee
    cert <- makeCert committee keys [0, 1] (leiosCertMessage $ lseAnnouncedInBlockHeader env)
    tryRunImpRule @"LEIOSCERT" env () cert
      `shouldReturn` Left [LeiosCertInsufficientWeight (Mismatch (1 / 2) (3 / 4))]

  it "Rejects a signature for a different header hash" $ do
    (env, cert) <- fixture
    otherHash <- arbitrary
    tryRunImpRule @"LEIOSCERT" env {lseAnnouncedInBlockHeader = otherHash} () cert
      `shouldReturn` Left [LeiosCertInvalidSignature]

  it "Rejects a signer without a key" $ do
    (env, _) <- fixture
    let (keys, committee) = testCommittee
    cert <- makeCert committee keys [0, 1, 2, 3] (leiosCertMessage $ lseAnnouncedInBlockHeader env)
    tryRunImpRule @"LEIOSCERT" env () cert
      `shouldReturn` Left [LeiosCertSignersWithoutKey [3]]

  it "Rejects a signer bitfield with the wrong length" $ do
    (env, _) <- fixture
    let (keys, committee) = mkCommitteeFromTestSeats $ replicate 9 (WithKey, 1 / 9)
    cert <- makeCert committee keys [0, 1, 2] (leiosCertMessage $ lseAnnouncedInBlockHeader env)
    tryRunImpRule @"LEIOSCERT" env () cert
      `shouldReturn` Left [LeiosCertMalformedSigners]

  it "Rejects a committee whose seats cannot be addressed" $ do
    (env, cert) <- fixture
    let (_, committee) = mkCommitteeFromTestSeats $ replicate 65_537 (NoKey, 1 / 65_537)
    tryRunImpRule @"LEIOSCERT" env {lseLeiosCommittee = committee} () cert
      `shouldReturn` Left [LeiosCertMalformedCommittee 65_537]

  it "Skips signature verification when a timing check fails" $ do
    (env, cert) <- fixture
    otherHash <- arbitrary
    let cur = lseCurrentSlotNo env - 1
    tryRunImpRule @"LEIOSCERT"
      env {lseCurrentSlotNo = cur, lseAnnouncedInBlockHeader = otherHash}
      ()
      cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch cur (lseCurrentSlotNo env))]

  it "Accepts a signature over the raw header hash" $ do
    (env, _) <- fixture
    let (keys, committee) = testCommittee
    cert <-
      makeCert committee keys [0, 1, 2] $ hashToBytes $ extractHash $ lseAnnouncedInBlockHeader env
    tryRunImpRule @"LEIOSCERT" env () cert `shouldReturn` Right ((), [])
