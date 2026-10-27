{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.Imp.SubDelegSpec (spec) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (DijkstraSubDelegPredFailure (..))
import Cardano.Ledger.Dijkstra.State
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.LedgerState (
  curPParamsEpochStateL,
  nesEsL,
 )
import Cardano.Ledger.Shelley.Rules (AccountAlreadyRegistered (..))
import Cardano.Ledger.Val (Val (..))
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common (
  Positive (..),
  SpecWith,
  describe,
  it,
 )
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common (arbitrary)
import Test.Cardano.Ledger.Plutus.Examples (evenRedeemerNoDatum)

spec ::
  forall era.
  DijkstraEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = describe "SUBDELEG" $ do
  describe "Register stake credential" $ do
    it "With incorrect deposit" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      Positive n <- arbitrary
      let wrongDeposit = expectedDeposit <+> Coin n

      freshKeyHash >>= \kh -> do
        submitFailingTx
          ( mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL .~ [RegDepositTxCert (KeyHashObj kh) wrongDeposit]
              ]
          )
          [ injectFailure . DijkstraSubDelegPredFailure $
              DepositIncorrectDELEG
                Mismatch
                  { mismatchSupplied = wrongDeposit
                  , mismatchExpected = expectedDeposit
                  }
          ]
        expectStakeCredNotRegistered (KeyHashObj kh)

    it "Fails when the credential is already registered" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [regTxCert]
          ]
      expectStakeCredRegistered cred
      regTxCert2 <- genRegTxCert cred
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [regTxCert2]
            ]
        )
        [ injectFailure . DijkstraSubDelegPredFailure . DelegAccountAlreadyRegistered $
            AccountAlreadyRegistered cred
        ]

  describe "Unregister stake credentials" $ do
    it "Fails when the credential is not registered" $ do
      cred <- KeyHashObj <$> freshKeyHash
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert cred expectedDeposit]
            ]
        )
        [injectFailure . DijkstraSubDelegPredFailure $ StakeKeyNotRegisteredDELEG cred]

    it "Fails when the account has a non-zero balance" $ do
      modifyPParams $ ppGovActionDepositL .~ Coin 3
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [regTxCert]
          ]
      submitAndExpireProposalToMakeReward cred
      balance <- getBalance cred
      unRegTxCert <- genUnRegTxCert cred
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [unRegTxCert]
            ]
        )
        [ injectFailure . DijkstraSubDelegPredFailure $
            StakeKeyHasNonZeroAccountBalanceDELEG balance
        ]
      expectStakeCredRegistered cred

    it "With incorrect refund" $ do
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      let cred = ScriptHashObj $ hashPlutusScript $ evenRedeemerNoDatum SPlutusV4

      impAnn "Register stake credential" $
        submitTx_ $
          mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [RegDepositTxCert cred expectedDeposit]
            ]

      Positive n <- arbitrary
      let wrongDeposit = expectedDeposit <+> Coin n

      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL
                  . certsTxBodyL
                  .~ [UnRegDepositTxCert cred wrongDeposit]
            ]
        )
        [ injectFailure . DijkstraSubDelegPredFailure $
            RefundIncorrectDELEG
              Mismatch
                { mismatchSupplied = wrongDeposit
                , mismatchExpected = expectedDeposit
                }
        ]

      expectStakeCredRegistered cred

  describe "Delegate stake" $ do
    it "Delegate to unregistered pool" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL
                . certsTxBodyL
                .~ [regTxCert]
          ]

      poolKh <- freshKeyHash
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL
                  . certsTxBodyL
                  .~ [DelegTxCert cred (DelegStake poolKh)]
            ]
        )
        [injectFailure . DijkstraSubDelegPredFailure $ DelegateeStakePoolNotRegisteredDELEG poolKh]
      expectNotDelegatedToAnyPool cred

  describe "Delegate vote" $ do
    it "Delegate vote of registered stake credentials to unregistered drep" $ do
      AccountAddress _ (AccountId cred) <- registerAccountAddress
      drepCred <- KeyHashObj <$> freshKeyHash
      let tx =
            mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL
                    . certsTxBodyL
                    .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred))]
              ]
      submitFailingTx
        tx
        [injectFailure . DijkstraSubDelegPredFailure $ DelegateeDRepNotRegisteredDELEG drepCred]
      expectNotDelegatedVote cred

    it "Delegate vote of unregistered stake credentials" $ do
      cred <- KeyHashObj <$> freshKeyHash
      drepCred <- KeyHashObj <$> registerDRep
      submitFailingTx
        ( mkTopTxWithSubTxs
            [ mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL .~ [DelegTxCert cred (DelegVote (DRepCredential drepCred))]
            ]
        )
        [injectFailure . DijkstraSubDelegPredFailure $ StakeKeyNotRegisteredDELEG cred]

      expectStakeCredNotRegistered cred

  describe "Delegate both stake and vote" $ do
    it "Fails to delegate to unregistered delegatees" $ do
      cred <- KeyHashObj <$> freshKeyHash
      regTxCert <- genRegTxCert cred
      submitTx_ $
        mkTopTxWithSubTxs
          [ mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [regTxCert]
          ]
      poolKh <- freshKeyHash
      drepCred <- KeyHashObj <$> freshKeyHash
      let tx =
            mkTopTxWithSubTxs
              [ mkBasicTx mkBasicTxBody
                  & bodyTxL . certsTxBodyL
                    .~ [DelegTxCert cred (DelegStakeVote poolKh (DRepCredential drepCred))]
              ]
          poolFailure =
            injectFailure . DijkstraSubDelegPredFailure $
              DelegateeStakePoolNotRegisteredDELEG poolKh
          drepFailure =
            injectFailure . DijkstraSubDelegPredFailure $
              DelegateeDRepNotRegisteredDELEG drepCred
      submitFailingTx tx [poolFailure, drepFailure]
      expectNotDelegatedToAnyPool cred
