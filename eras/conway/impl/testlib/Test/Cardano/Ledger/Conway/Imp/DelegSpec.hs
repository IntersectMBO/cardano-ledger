{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Conway.Imp.DelegSpec (
  spec,
) where

import Cardano.Ledger.BaseTypes (EpochInterval (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance (GovPurposeId (..), Voter (..))
import Cardano.Ledger.Conway.Rules (ConwayDelegPredFailure (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Plutus (
  Data (..),
  ExUnits (..),
  Language (..),
  SLanguage (..),
  hashData,
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Shelley.LedgerState -- (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.UMap as UMap
import Cardano.Ledger.Val (Val (..))
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Debug.Trace
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational (IsRatio (..))
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysSucceedsNoDatum,
  evenRedeemerNoDatum,
  redeemerSameAsDatum,
 )

spec ::
  forall era.
  ( ConwayEraImp era
  , InjectRuleFailure "LEDGER" ConwayDelegPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = do
  describe "xxx" $ do
    describe "Register stake credential" $ do
      it "With correct deposit or without any deposit" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

        freshKeyHash >>= \kh -> do
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [RegTxCert @era (KeyHashObj kh)]
          expectInRDMap (KeyHashObj kh)

        freshKeyHash >>= \kh -> do
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [RegDepositTxCert @era (KeyHashObj kh) expectedDeposit]
          expectInRDMap (KeyHashObj kh)

      it "Twice the same certificate in the same transaction" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
        freshKeyHash >>= \kh -> do
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegDepositTxCert @era (KeyHashObj kh) expectedDeposit
                   , RegDepositTxCert @era (KeyHashObj kh) expectedDeposit
                   ]
          expectInRDMap (KeyHashObj kh)

      it "When already already registered" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
        let sh = hashPlutusScript (evenRedeemerNoDatum SPlutusV3)
        let tx =
              mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [RegDepositTxCert @era (ScriptHashObj sh) expectedDeposit]
        submitTx_ tx

        submitFailingTx
          tx
          [ injectFailure $ StakeKeyRegisteredDELEG (ScriptHashObj sh)
          ]
        expectInRDMap (ScriptHashObj sh)

      it "With incorrect deposit" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

        Positive n <- arbitrary
        let wrongDeposit = expectedDeposit <+> Coin n

        freshKeyHash >>= \kh -> do
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [RegDepositTxCert @era (KeyHashObj kh) wrongDeposit]
            )
            [injectFailure $ IncorrectDepositDELEG wrongDeposit]
          expectNotInRDMap (KeyHashObj kh)

    describe "Unregister stake credentials" $ do
      it "When registered" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
        let sh = ScriptHashObj $ hashPlutusScript (evenRedeemerNoDatum SPlutusV3)
        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [RegDepositTxCert @era sh expectedDeposit]

        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [UnRegDepositTxCert @era sh expectedDeposit]
        expectNotInRDMap sh

      it "When not registered" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
        freshKeyHash >>= \kh ->
          submitFailingTx
            ( mkBasicTx mkBasicTxBody
                & bodyTxL . certsTxBodyL
                  .~ [UnRegDepositTxCert @era (KeyHashObj kh) expectedDeposit]
            )
            [ injectFailure $ StakeKeyNotRegisteredDELEG (KeyHashObj kh)
            , injectFailure $ IncorrectDepositDELEG expectedDeposit
            ]

      it "With incorrect deposit" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

        cred <- KeyHashObj <$> freshKeyHash

        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [RegDepositTxCert @era cred expectedDeposit]

        Positive n <- arbitrary
        let wrongDeposit = expectedDeposit <+> Coin n

        submitFailingTx
          ( mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [UnRegDepositTxCert @era cred wrongDeposit]
          )
          [injectFailure $ IncorrectDepositDELEG wrongDeposit]

        expectInRDMap cred

      it "With non-zero reward balance" $ do
        modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

        cred <- KeyHashObj <$> freshKeyHash

        submitTx_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL .~ [RegDepositTxCert @era cred expectedDeposit]

        submitAndExpireProposalToMakeReward cred

        reward <- lookupReward cred
        submitFailingTx
          ( mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL .~ [UnRegDepositTxCert @era cred expectedDeposit]
          )
          [injectFailure $ StakeKeyHasNonZeroRewardAccountBalanceDELEG reward]
        expectInRDMap cred

      it "Register and unregister in the same transaction" $ do
        expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL
        freshKeyHash >>= \kh -> do
          submitTx_ $
            mkBasicTx mkBasicTxBody
              & bodyTxL . certsTxBodyL
                .~ [ RegDepositTxCert @era (KeyHashObj kh) expectedDeposit
                   , UnRegDepositTxCert @era (KeyHashObj kh) expectedDeposit
                   ]
          expectNotInRDMap (KeyHashObj kh)
  where
    expectInRDMap cred = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      expectedDeposit <- getsNES $ nesEsL . curPParamsEpochStateL . ppKeyDepositL

      let umapDeposit = rdDepositCoin <$> UMap.lookup cred (RewDepUView umap)
      impAnn
        (show cred <> " expected to be in UMap RewDep with the correct deposit")
        $ umapDeposit `shouldBe` Just expectedDeposit

    expectNotInRDMap cred = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      impAnn (show cred <> " expected to not be in UMap RewDep") $
        UMap.notMember cred (RewDepUView umap) `shouldBe` True

    printUMap = do
      umap <- getsNES $ nesEsL . esLStateL . lsCertStateL . certDStateL . dsUnifiedL
      let !_ = trace ("\n UMAP IS:" <> (show umap) <> "\n") True
      let !_ = trace ("\n VIEWS:" <> (show "----------------") <> "\n") True
      let !_ = trace ("\n 1" <> (show (unUView (RewDepUView umap))) <> "\n") True
      let !_ = trace ("\n 2" <> (show (unUView (PtrUView umap))) <> "\n") True
      let !_ = trace ("\n 3" <> (show (unUView (SPoolUView umap))) <> "\n") True
      let !_ = trace ("\n 4" <> (show (unUView (DRepUView umap))) <> "\n") True

      pure ()
