{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LeiosCertSpec (spec, dijkstraOnlySpec) where

import Cardano.Crypto.DSIGN (signDSIGN)
import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Crypto.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (
  Block,
  blockHeaderHashBlockHeaderG,
  ebReferencesAnnouncementBlockHeaderL,
  slotNoBlockHeaderL,
 )
import Cardano.Ledger.Core (injectFailure)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraEraBlockBody (..))
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Hashes (extractHash)
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  esSnapshotsL,
  lsLastEbAnnouncementL,
  nesELL,
  nesEsL,
 )
import Cardano.Ledger.Slot (epochInfoFirst)
import Cardano.Ledger.State (EbAnnouncement (..), ssLeiosCommitteeL, ssStakeSetL)
import Cardano.Slotting.EpochInfo (EpochInfo (epochInfoSlotLength_))
import Cardano.Slotting.Time (mkSlotLength)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word32)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl (use, (%=))
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Cardano.Ledger.BlockHeader (TestBlockHeader)
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

  it "Preserves fractional milliseconds in the slot length" $ do
    (env, cert) <- fixture
    impGlobalsL %= \globals ->
      globals
        { epochInfo =
            (epochInfo globals) {epochInfoSlotLength_ = const $ pure $ mkSlotLength (3 / 2000)}
        }
    let annSlot = lseAnnouncementSlotNo env
        env' =
          env
            { lseCurrentSlotNo = annSlot + 2
            , lseAnnouncementPeriodLength = 0
            , lseVotePeriodLength = 3
            , lseDiffusionPeriodLength = 0
            }
    tryRunImpRule @"LEIOSCERT" env' () cert `shouldReturn` Right ((), [])
    tryRunImpRule @"LEIOSCERT" env' {lseCurrentSlotNo = annSlot + 1} () cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch (annSlot + 1) (annSlot + 2))]

  it "Uses the slot length at the certifying slot" $ do
    (env, cert) <- fixture
    let annSlot = lseAnnouncementSlotNo env
        cur = lseCurrentSlotNo env - 1
    impGlobalsL %= \globals ->
      globals
        { epochInfo =
            (epochInfo globals)
              { epochInfoSlotLength_ = \slot -> pure $ mkSlotLength $ if slot == annSlot then 10 else 1
              }
        }
    tryRunImpRule @"LEIOSCERT" env {lseCurrentSlotNo = cur} () cert
      `shouldReturn` Left [LeiosCertTooEarly (Mismatch cur (lseCurrentSlotNo env))]

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

-- Set the committee and timing explicitly: these tests do not depend on the
-- example genesis's Leios defaults.
setupCommittee :: DijkstraEraImp era => ImpTestM era ()
setupCommittee = do
  let (_, committee) = testCommittee
  modifyNES $ nesEsL . esSnapshotsL . ssStakeSetL . ssLeiosCommitteeL .~ committee
  modifyPParams $ \pp ->
    pp
      & ppLeiosQuorumStakeThresholdL
        .~ (3 %! 4)
      & ppLeiosAnnouncementPeriodLengthL
        .~ Milliseconds32 1_000
      & ppLeiosVotePeriodLengthL
        .~ Milliseconds32 4_000
      & ppLeiosDiffusionPeriodLengthL
        .~ Milliseconds32 7_000

announceAndSign ::
  DijkstraEraImp era =>
  ImpTestM era (Block TestBlockHeader era, LeiosCert)
announceAndSign = do
  setupCommittee
  ebRefs <- arbitrary
  block <- withTxsInModifiedBlock (ebReferencesAnnouncementBlockHeaderL .~ SJust ebRefs) (pure ())
  let (keys, committee) = testCommittee
  cert <- makeCert committee keys [0, 1, 2] $ leiosCertMessage $ block ^. blockHeaderHashBlockHeaderG
  pure (block, cert)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEIOSCERT in BBODY" $ do
  it "Accepts the certificate for the preceding block's announcement" $ do
    (_, cert) <- announceAndSign
    replicateM_ 14 passTick
    protVer <- getProtVer
    _ <- withTxsInModifiedBlock (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert) (pure ())
    getsNES (nesEsL . esLStateL . lsLastEbAnnouncementL) `shouldReturn` SNothing

  it "Rejects a certificate without an announcement" $ do
    setupCommittee
    cert <- arbitrary
    protVer <- getProtVer
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert)
      (pure ())
      (const $ pure [injectFailure LeiosCertWithoutEbAnnouncement])

  it "Discards an announcement when an intervening block announces nothing" $ do
    (_, cert) <- announceAndSign
    replicateM_ 14 passTick
    withTxsInBlock_ (pure ())
    protVer <- getProtVer
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert)
      (pure ())
      (const $ pure [injectFailure LeiosCertWithoutEbAnnouncement])

  it "Rejects a second certification of the same announcement" $ do
    (_, cert) <- announceAndSign
    replicateM_ 14 passTick
    protVer <- getProtVer
    let certify = modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert
    _ <- withTxsInModifiedBlock certify (pure ())
    passTick
    withTxsInModifiedFailingBlockM
      certify
      (pure ())
      (const $ pure [injectFailure LeiosCertWithoutEbAnnouncement])

  it "Propagates a LEIOSCERT failure" $ do
    (announcement, cert) <- announceAndSign
    passTick
    protVer <- getProtVer
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert)
      (pure ())
      ( \block ->
          pure
            [ injectFailure $
                LeiosCertTooEarly $
                  Mismatch
                    (block ^. slotNoBlockHeaderL)
                    (announcement ^. slotNoBlockHeaderL + 14)
            ]
      )

  it "Records its own announcement when certifying the preceding block" $ do
    (_, cert) <- announceAndSign
    replicateM_ 14 passTick
    newEbRefs <- arbitrary
    protVer <- getProtVer
    block <-
      withTxsInModifiedBlock
        ( (ebReferencesAnnouncementBlockHeaderL .~ SJust newEbRefs)
            . modifyBlockBody protVer (leiosCertBlockBodyL .~ SJust cert)
        )
        (pure ())
    getsNES (nesEsL . esLStateL . lsLastEbAnnouncementL)
      `shouldReturn` SJust
        EbAnnouncement
          { ebAnnouncement = newEbRefs
          , ebAnnouncementSlotNo = block ^. slotNoBlockHeaderL
          , ebAnnouncedInBlockHeader = block ^. blockHeaderHashBlockHeaderG
          }
