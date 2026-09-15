{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubGovCertSpec (spec) where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Mismatch (..), StrictMaybe (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  DijkstraGovCertPredFailure (..),
  DijkstraSubGovCertPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.State (DRepState (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBGOVCERT" $ do
  describe "DRep registration" $ do
    it "Registering a DRep succeeds" $ do
      void registerDRepSubTx
    it "Updating a DRep succeeds" $ do
      drepCred <- registerDRepSubTx
      newAnchor <- arbitrary
      submitTxAnn_ "Updating the DRep" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [UpdateDRepTxCert drepCred newAnchor]
          ]
      dSt <- lookupDRepState drepCred
      case dSt of
        Just DRepState {..} -> drepAnchor `shouldBeExpr` newAnchor
        Nothing -> expectationFailure "Expected DRep to exist, but it does not"
    it "Fails to update an unregistered DRep" $ do
      drepCred <- KeyHashObj <$> freshKeyHash
      anchor <- arbitrary
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [UpdateDRepTxCert drepCred anchor]
            ]
        )
        [ injectFailure . DijkstraSubGovCertPredFailure $ DijkstraDRepNotRegistered drepCred
        ]
    it "Fails to register with an invalid deposit" $ do
      drepCred <- KeyHashObj <$> freshKeyHash
      expectedDeposit <- getsPParams ppDRepDepositL
      anchor <- arbitrary
      deposit <- arbitrary `suchThat` (/= expectedDeposit)
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [RegDRepTxCert drepCred deposit anchor]
            ]
        )
        [ injectFailure . DijkstraSubGovCertPredFailure . DijkstraDRepIncorrectDeposit $
            Mismatch deposit expectedDeposit
        ]
    describe "Fails to register an already existing DRep" $ do
      it "With correct deposit" $ do
        drepCred <- registerDRepSubTx
        drepDeposit <- getsPParams ppDRepDepositL
        anchor2 <- arbitrary
        submitFailingTx
          ( mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ [RegDRepTxCert drepCred drepDeposit anchor2]
              ]
          )
          [ injectFailure . DijkstraSubGovCertPredFailure $ DijkstraDRepAlreadyRegistered drepCred
          ]
      it "With incorrect deposit" $ do
        drepCred <- registerDRepSubTx
        drepDeposit1 <- getsPParams ppDRepDepositL
        drepDeposit2 <-
          frequency
            [ (1, pure mempty)
            , (99, arbitrary `suchThat` (/= drepDeposit1))
            ]
        anchor2 <- arbitrary
        expectDRepRegistered drepCred
        submitFailingTx
          ( mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ [RegDRepTxCert drepCred drepDeposit2 anchor2]
              ]
          )
          [ injectFailure . DijkstraSubGovCertPredFailure . DijkstraDRepIncorrectDeposit $
              Mismatch drepDeposit2 drepDeposit1
          , injectFailure . DijkstraSubGovCertPredFailure $ DijkstraDRepAlreadyRegistered drepCred
          ]
  describe "DRep deregistration" $ do
    it "Deregistering an existing DRep succeeds" $ do
      drepCred <- registerDRepSubTx
      drepDeposit <- getsPParams ppDRepDepositL
      submitTxAnn_ "Deregistering the DRep" $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [UnRegDRepTxCert drepCred drepDeposit]
          ]
      expectDRepNotRegistered drepCred
    it "Fails when trying to deregister a nonexistent DRep" $ do
      drepCred <- KeyHashObj <$> freshKeyHash
      drepDeposit <-
        frequency
          [ (1, getsPParams ppDRepDepositL)
          , (99, arbitrary)
          ]
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [UnRegDRepTxCert drepCred drepDeposit]
            ]
        )
        [ injectFailure . DijkstraSubGovCertPredFailure $ DijkstraDRepNotRegistered drepCred
        ]
    it "Fails when deregistering with invalid deposit" $ do
      drepCred <- registerDRepSubTx
      expectedDeposit <- getsPParams ppDRepDepositL
      actualDeposit <- arbitrary `suchThat` (/= expectedDeposit)
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [UnRegDRepTxCert drepCred actualDeposit]
            ]
        )
        [ injectFailure . DijkstraSubGovCertPredFailure . DijkstraDRepIncorrectRefund $
            Mismatch actualDeposit expectedDeposit
        ]
  describe "Committee hot key authorization" $ do
    it "Authorizing a hot key succeeds" $ do
      void registerInitialCommittee
      initialCommittee <- getCommitteeMembers
      forM_ initialCommittee $ \ccColdCred ->
        replicateM_ 2 $ do
          ccHotCred <- KeyHashObj <$> freshKeyHash
          submitTxAnn_ "Authorizing a hot key" $
            mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ [AuthCommitteeHotKeyTxCert ccColdCred ccHotCred]
              ]
    it "Fails for an unknown cold credential until it is proposed as a member" $
      whenPostBootstrap $ do
        unknownColdCred <- KeyHashObj <$> freshKeyHash
        ccHotCred <- KeyHashObj <$> freshKeyHash
        let authTx =
              mkTopTxWithSubTxs
                [ mkBasicTx mkBasicTxBody
                    & bodyTxL . certsTxBodyL .~ [AuthCommitteeHotKeyTxCert unknownColdCred ccHotCred]
                ]
        submitFailingTx
          authTx
          [ injectFailure . DijkstraSubGovCertPredFailure $ DijkstraCommitteeIsUnknown unknownColdCred
          ]
        void $ submitUpdateCommittee Nothing mempty [(unknownColdCred, EpochInterval 20)] (1 %! 2)
        submitTx_ authTx
    it "Fails for a resigned member" $ do
      void registerInitialCommittee
      initialCommittee <- getCommitteeMembers
      forM_ initialCommittee $ \ccColdCred -> do
        ccHotCred <- KeyHashObj <$> freshKeyHash
        let authTx =
              mkTopTxWithSubTxs
                [ mkBasicTx mkBasicTxBody
                    & bodyTxL . certsTxBodyL .~ [AuthCommitteeHotKeyTxCert ccColdCred ccHotCred]
                ]
        submitTxAnn_ "Authorizing a hot key" authTx
        submitTxAnn_ "Resigning the cold key" $
          mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [ResignCommitteeColdTxCert ccColdCred SNothing]
            ]
        submitFailingTx
          authTx
          [ injectFailure . DijkstraSubGovCertPredFailure $
              DijkstraCommitteeHasPreviouslyResigned ccColdCred
          ]
  describe "Committee cold key resignation" $ do
    it "Resigning with an anchor succeeds" $ do
      void registerInitialCommittee
      initialCommittee <- getCommitteeMembers
      forM_ initialCommittee $ \ccColdCred -> do
        anchor <- arbitrary
        submitTxAnn_ "Resigning the cold key" $
          mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [ResignCommitteeColdTxCert ccColdCred anchor]
            ]
    it "Resigning a proposed member succeeds" $
      whenPostBootstrap $ do
        ccColdCred <- KeyHashObj <$> freshKeyHash
        void $ submitUpdateCommittee Nothing mempty [(ccColdCred, EpochInterval 20)] (1 %! 2)
        submitTxAnn_ "Resigning the proposed cold key" $
          mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [ResignCommitteeColdTxCert ccColdCred SNothing]
            ]
    it "Fails for an unknown cold credential" $ do
      void registerInitialCommittee
      unknownColdCred <- KeyHashObj <$> freshKeyHash
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [ResignCommitteeColdTxCert unknownColdCred SNothing]
            ]
        )
        [ injectFailure . DijkstraSubGovCertPredFailure $ DijkstraCommitteeIsUnknown unknownColdCred
        ]
    it "Fails for a member that has already resigned" $ do
      void registerInitialCommittee
      initialCommittee <- getCommitteeMembers
      forM_ initialCommittee $ \ccColdCred -> do
        let resignTx =
              mkTopTxWithSubTxs
                [ mkBasicTx mkBasicTxBody
                    & bodyTxL . certsTxBodyL .~ [ResignCommitteeColdTxCert ccColdCred SNothing]
                ]
        submitTxAnn_ "Resigning the cold key" resignTx
        submitFailingTx
          resignTx
          [ injectFailure . DijkstraSubGovCertPredFailure $
              DijkstraCommitteeHasPreviouslyResigned ccColdCred
          ]
