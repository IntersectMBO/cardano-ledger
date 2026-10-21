{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.BbodySpec (spec) where

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
import Cardano.Ledger.Dijkstra.Rules (DijkstraBbodyPredFailure (..))
import Cardano.Ledger.Shelley.LedgerState (esLStateL, lsLastEbAnnouncementL, nesEsL)
import Cardano.Ledger.State (EbAnnouncement (..))
import Lens.Micro ((%~), (.~), (^.))
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
