{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.BbodySpec (spec) where

import Cardano.Crypto.Leios (LeiosCert)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block (
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
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.PParams (
  ppMaxRefScriptSizePerBlockL,
  ppMaxRefScriptSizePerTxL,
 )
import Cardano.Ledger.Dijkstra.Rules (DijkstraBbodyPredFailure (..))
import Cardano.Ledger.Plutus (SLanguage (..))
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsLastEbAnnouncementL, nesEsL)
import Cardano.Ledger.State (EbAnnouncement (..))
import Lens.Micro ((%~), (&), (.~), (^.))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (purposeIsWellformedNoDatum)

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

  it "BodyRefScriptsSizeTooBig counts reference scripts of sub-transactions" $ do
    plutusScript <- mkPlutusScript @era $ purposeIsWellformedNoDatum SPlutusV3
    let script = fromPlutusScript plutusScript
        refScriptSize = originalBytesSize script
    refTxIn <- produceRefScript script

    -- Each of the three top-level transactions references the script once itself and once
    -- from its sub-transaction, so each accounts for twice the script size and stays
    -- within the per-transaction limit.
    -- Together they stay within the per-block limit as long as only top-level transactions are counted
    -- and exceed it as soon as the sub-transactions are counted as well.
    modifyPParams $ \pp ->
      pp
        & ppMaxRefScriptSizePerTxL .~ fromIntegral (4 * refScriptSize)
        & ppMaxRefScriptSizePerBlockL .~ fromIntegral (5 * refScriptSize)

    let subTx :: Tx SubTx era
        subTx = mkBasicTx $ mkBasicTxBody & referenceInputsTxBodyL .~ [refTxIn]
        mkTopTx = do
          topTx <- mkTopTxWithDistinctSubTxs [subTx]
          pure $ topTx & bodyTxL . referenceInputsTxBodyL .~ [refTxIn]
    txs <- replicateM 3 mkTopTx
    submitFailingBlock
      txs
      [ injectFailure $
          BodyRefScriptsSizeTooBig
            Mismatch
              { mismatchSupplied = 6 * refScriptSize
              , mismatchExpected = 5 * refScriptSize
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
      cert <- announceCertifiableEb
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

  it "Cannot certify the same EB twice" $ do
    cert <- announceCertifiableEb
    replicateM_ 14 passTick
    protVer <- getProtVer
    let certify = modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert
    _ <- withTxsInModifiedBlock certify (pure ())
    passTick
    withTxsInModifiedFailingBlockM certify (pure ()) $
      \_ -> pure [injectFailure LeiosCertWithoutEbAnnouncement]

-- | Seats a Leios committee, announces an EB and certifies it with enough votes for the quorum.
-- The certificate can be included 14 slots later.
announceCertifiableEb :: DijkstraEraImp era => ImpTestM era LeiosCert
announceCertifiableEb = do
  signKeys <- setupLeiosCommittee 4
  ebRefsAnn <- arbitrary
  block <- withTxsInModifiedBlock (ebReferencesAnnouncementBlockHeaderL .~ SJust ebRefsAnn) (pure ())
  mkLeiosCert (take 3 signKeys) $ block ^. blockHeaderHashBlockHeaderG
