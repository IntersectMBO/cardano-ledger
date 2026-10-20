{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.Imp.LeiosCertSpec (spec) where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash.Class
import Cardano.Crypto.Leios
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Block
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.BlockBody
import Cardano.Ledger.Dijkstra.PParams
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State
import Data.List.NonEmpty (NonEmpty)
import Lens.Micro
import Test.Cardano.Ledger.BlockHeader
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "LEIOSCERT" $ do
  it "Certifies the EB announced by the preceding block" $ do
    signKeys <- setupLeiosCommittee 4
    announcingBlock <- announceEb
    cert <- mkLeiosCert (take 3 signKeys) $ announcingBlock ^. blockHeaderHashBlockHeaderG
    replicateM_ 14 passTick
    certifyInBlock cert

  it "LeiosCertTooEarly" $ do
    signKeys <- setupLeiosCommittee 4
    announcingBlock <- announceEb
    cert <- mkLeiosCert (take 3 signKeys) $ announcingBlock ^. blockHeaderHashBlockHeaderG
    let announcementSlot = announcingBlock ^. slotNoBlockHeaderL
    replicateM_ 13 passTick
    certifyInFailingBlock
      cert
      [injectFailure $ LeiosCertTooEarly (Mismatch (announcementSlot + 13) (announcementSlot + 14))]
    passTick
    certifyInBlock cert

  it "Rounds the certification delay up to whole slots" $ do
    signKeys <- setupLeiosCommittee 4
    modifyPParams $ ppLeiosDiffusionPeriodLengthL .~ Milliseconds32 7_001
    announcingBlock <- announceEb
    cert <- mkLeiosCert (take 3 signKeys) $ announcingBlock ^. blockHeaderHashBlockHeaderG
    let announcementSlot = announcingBlock ^. slotNoBlockHeaderL
    replicateM_ 14 passTick
    certifyInFailingBlock
      cert
      [injectFailure $ LeiosCertTooEarly (Mismatch (announcementSlot + 14) (announcementSlot + 15))]
    passTick
    certifyInBlock cert

  it "LeiosCertAnnouncedInPreviousEpoch" $ do
    signKeys <- setupLeiosCommittee 4
    announcingBlock <- announceEb
    cert <- mkLeiosCert (take 3 signKeys) $ announcingBlock ^. blockHeaderHashBlockHeaderG
    announcementEpoch <- getsNES nesELL
    passEpoch
    currentEpoch <- getsNES nesELL
    certifyInFailingBlock
      cert
      [injectFailure $ LeiosCertAnnouncedInPreviousEpoch (Mismatch currentEpoch announcementEpoch)]

  it "LeiosCertInsufficientWeight" $ do
    signKeys <- setupLeiosCommittee 4
    announcingBlock <- announceEb
    cert <- mkLeiosCert (take 2 signKeys) $ announcingBlock ^. blockHeaderHashBlockHeaderG
    replicateM_ 14 passTick
    certifyInFailingBlock
      cert
      [injectFailure $ LeiosCertInsufficientWeight (Mismatch (1 / 2) (3 / 4))]

  it "LeiosCertInvalidSignature" $ do
    signKeys <- setupLeiosCommittee 4
    _ <- announceEb
    otherHeaderHash <- arbitrary
    cert <- mkLeiosCert (take 3 signKeys) otherHeaderHash
    replicateM_ 14 passTick
    certifyInFailingBlock cert [injectFailure LeiosCertInvalidSignature]

  it "LeiosCertSignersWithoutKey" $ do
    -- a pool that registers no BLS key is seated without one
    _ <- setupPoolWithStake $ Coin 1_000_000_000
    passNEpochs 2
    announcingBlock <- announceEb
    committee <- getsNES $ nesEsL . esSnapshotsL . ssStakeSetL . ssLeiosCommitteeL
    signKey <- arbitrary
    let msg = hashToBytes . extractHash $ announcingBlock ^. blockHeaderHashBlockHeaderG
    cert <-
      expectRightDeep $
        aggregateLeiosCert committee [(LeiosSeatId 0, signDSIGN leiosSignContext msg signKey)]
    replicateM_ 14 passTick
    certifyInFailingBlock cert [injectFailure $ LeiosCertSignersWithoutKey [0]]

announceEb :: DijkstraEraImp era => ImpTestM era (Block TestBlockHeader era)
announceEb = do
  ebRefsAnn <- arbitrary
  withTxsInModifiedBlock (ebReferencesAnnouncementBlockHeaderL .~ SJust ebRefsAnn) (pure ())

certifyInBlock :: DijkstraEraImp era => LeiosCert -> ImpTestM era ()
certifyInBlock cert = do
  protVer <- getProtVer
  void $
    withTxsInModifiedBlock (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert) (pure ())

certifyInFailingBlock ::
  DijkstraEraImp era =>
  LeiosCert ->
  NonEmpty (PredicateFailure (EraRule "BBODY" era)) ->
  ImpTestM era ()
certifyInFailingBlock cert expectedFailures = do
  protVer <- getProtVer
  withTxsInModifiedFailingBlockM
    (modifyBlockBody protVer $ leiosCertBlockBodyL .~ SJust cert)
    (pure ())
    (\_ -> pure expectedFailures)
