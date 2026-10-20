{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.BbodySpec (spec) where

import Cardano.Crypto.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (
  Block,
  BlockHeaderVersionInfo (..),
  blockBodyContainsLeiosCertBlockHeaderL,
  blockHeaderHashBlockHeaderG,
  ebReferencesAnnouncementBlockHeaderL,
  prevNonceBlockHeaderL,
  slotNoBlockHeaderL,
  versionInfoBlockHeaderL,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.BlockBody (DijkstraEraBlockBody (..))
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraBbodyPredFailure (..),
  DijkstraLeiosCertPredFailure (..),
 )
import Cardano.Ledger.Shelley.LedgerState (
  esLStateL,
  esSnapshotsL,
  lsLastEbAnnouncementL,
  nesEsL,
 )
import Cardano.Ledger.State (EbAnnouncement (..), ssLeiosCommitteeL, ssStakeSetL)
import Lens.Micro ((%~), (&), (.~), (^.))
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Cardano.Ledger.BlockHeader
import Test.Cardano.Ledger.Core.Rational
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "BBODY" $ do
  it "PerasCertValidationFailed" $ do
    protVer <- getProtVer
    perasCert <- arbitrary
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ perasCertBlockBodyL .~ SJust perasCert)
      (submitAnyTx_ $ mkBasicTx mkBasicTxBody)
      $ \block ->
        pure
          [ injectFailure $
              PerasCertValidationFailed perasCert $
                block ^. prevNonceBlockHeaderL
          ]

  it "HeaderProtVerTooLow" $ do
    ProtVer curMajor _ <- getProtVer
    withTxsInModifiedFailingBlockM
      ( versionInfoBlockHeaderL
          %~ \versionInfo ->
            versionInfo
              { bhviHighestSupportedMajorVersion =
                  pred $ bhviHighestSupportedMajorVersion versionInfo
              }
      )
      (submitAnyTx_ $ mkBasicTx mkBasicTxBody)
      $ \block ->
        pure
          [ injectFailure $
              HeaderProtVerTooLow
                Mismatch
                  { mismatchSupplied =
                      bhviHighestSupportedMajorVersion $ block ^. versionInfoBlockHeaderL
                  , mismatchExpected = getVersion32 curMajor
                  }
          ]

  describe "EB announcement" $ do
    let
      announceEb ebRefsAnn = ebReferencesAnnouncementBlockHeaderL .~ SJust ebRefsAnn
      expectLastEbAnnouncement block ebRefsAnn =
        getsNES (nesEsL . esLStateL . lsLastEbAnnouncementL)
          `shouldReturn` SJust
            EbAnnouncement
              { ebAnnouncement = ebRefsAnn
              , ebAnnouncementSlotNo = block ^. slotNoBlockHeaderL
              , ebAnnouncedInBlockHeader = block ^. blockHeaderHashBlockHeaderG
              }

    it "Records the announcement made in the block header" $ do
      ebRefsAnn <- arbitrary
      block <- withTxsInModifiedBlock (announceEb ebRefsAnn) (pure ())
      expectLastEbAnnouncement block ebRefsAnn

    it "Replaces it with a newer announcement" $ do
      ebRefsAnn1 <- arbitrary
      _ <- withTxsInModifiedBlock (announceEb ebRefsAnn1) (pure ())
      passTick
      ebRefsAnn2 <- arbitrary
      block2 <- withTxsInModifiedBlock (announceEb ebRefsAnn2) (pure ())
      expectLastEbAnnouncement block2 ebRefsAnn2

    it "Clears it when a later block announces nothing" $ do
      ebRefsAnn <- arbitrary
      block <- withTxsInModifiedBlock (announceEb ebRefsAnn) (pure ())
      expectLastEbAnnouncement block ebRefsAnn
      passTick
      withTxsInBlock_ (pure ())
      getsNES (nesEsL . esLStateL . lsLastEbAnnouncementL) `shouldReturn` SNothing

    it "Records the announcement of a block that certifies an EB" $ do
      (_, cert) <- announceCertifiableEb
      replicateM_ 14 passTick
      protVer <- getProtVer
      ebRefsAnn <- arbitrary
      block <-
        withTxsInModifiedBlock
          (announceEb ebRefsAnn . modifyBlockBody protVer (leiosCertBlockBodyL .~ SJust cert))
          (pure ())
      expectLastEbAnnouncement block ebRefsAnn

  it "LeiosCertMissing"
    $ withTxsInModifiedFailingBlockM
      (blockBodyContainsLeiosCertBlockHeaderL .~ True)
      (submitAnyTx_ $ mkBasicTx mkBasicTxBody)
    $ \_ -> pure [injectFailure LeiosCertMissing]

  it "LeiosCertBlockBodyHasTxs" $ do
    protVer <- getProtVer
    leiosCert <- arbitrary
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust leiosCert)
      (submitAnyTx_ $ mkBasicTx mkBasicTxBody)
      $ \_ -> pure [injectFailure LeiosCertBlockBodyHasTxs]

  it "LeiosCertWithoutEbAnnouncement" $ do
    protVer <- getProtVer
    leiosCert <- arbitrary
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust leiosCert)
      (pure ())
      $ \_ -> pure [injectFailure LeiosCertWithoutEbAnnouncement]

  it "LeiosCertFailure" $ do
    (announcingBlock, cert) <- announceCertifiableEb
    passTick
    protVer <- getProtVer
    withTxsInModifiedFailingBlockM
      (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert)
      (pure ())
      $ \block ->
        pure
          [ injectFailure $
              LeiosCertTooEarly $
                Mismatch (block ^. slotNoBlockHeaderL) (announcingBlock ^. slotNoBlockHeaderL + 14)
          ]

  describe "EB certification" $ do
    it "Certifies the EB announced by the preceding block" $ do
      (_, cert) <- announceCertifiableEb
      replicateM_ 14 passTick
      protVer <- getProtVer
      void $
        withTxsInModifiedBlock (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert) (pure ())

    it "Cannot certify the same EB twice" $ do
      (_, cert) <- announceCertifiableEb
      replicateM_ 14 passTick
      protVer <- getProtVer
      let certify = modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert
      _ <- withTxsInModifiedBlock certify (pure ())
      passTick
      withTxsInModifiedFailingBlockM certify (pure ()) $
        \_ -> pure [injectFailure LeiosCertWithoutEbAnnouncement]

-- | Seat a committee of four seats, any three of which reach the quorum, announce an EB and
-- sign the announcement with three seats. The certificate can be included 14 slots later.
announceCertifiableEb ::
  DijkstraEraImp era => ImpTestM era (Block TestBlockHeader era, LeiosCert)
announceCertifiableEb = do
  let (keys, committee) = mkCommitteeFromTestSeats $ replicate 4 (WithKey, 1 / 4)
  modifyNES $ nesEsL . esSnapshotsL . ssStakeSetL . ssLeiosCommitteeL .~ committee
  modifyPParams $ \pp ->
    pp
      & ppLeiosQuorumStakeThresholdL .~ (3 %! 4)
      & ppLeiosAnnouncementPeriodLengthL .~ Milliseconds32 1_000
      & ppLeiosVotePeriodLengthL .~ Milliseconds32 4_000
      & ppLeiosDiffusionPeriodLengthL .~ Milliseconds32 7_000
  ebRefsAnn <- arbitrary
  block <- withTxsInModifiedBlock (ebReferencesAnnouncementBlockHeaderL .~ SJust ebRefsAnn) (pure ())
  cert <- mkLeiosCert committee keys [0, 1, 2] $ block ^. blockHeaderHashBlockHeaderG
  pure (block, cert)
