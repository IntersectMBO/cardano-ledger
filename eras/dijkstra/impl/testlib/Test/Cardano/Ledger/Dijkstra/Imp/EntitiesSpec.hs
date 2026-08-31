{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.EntitiesSpec (spec) where

import Cardano.Base.Typeable (Typeable)
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Rules (
  EntitiesPredFailure (..),
  SubEntitiesPredFailure (..),
 )
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval (..), AccountBalanceIntervals (..))
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe (fromJust)
import qualified Data.OMap.Strict as OMap
import qualified Data.Set.NonEmpty as NES
import Data.Word (Word64)
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Dijkstra.ImpTest
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "ENTITIES" $ do
  it "Batch with successful withdrawals and direct deposits" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (acc1, reward1, kh1) <- setupAccountAddress
    (acc2, reward2, kh2) <- setupAccountAddress
    (acc3, reward3, kh3) <- setupAccountAddress

    let depositAmount = Coin 50
        partialWithdrawal = reward3 <-> Coin 10
        subDeposit =
          mkBasicTx $
            mkBasicTxBody
              & directDepositsTxBodyL .~ DirectDeposits [(acc1, depositAmount)]
        subWithdraw =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL .~ Withdrawals [(acc2, reward2)]
        topTx =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL .~ Withdrawals [(acc3, partialWithdrawal)]
              & subTransactionsTxBodyL .~ [subDeposit, subWithdraw]
    submitTx_ topTx

    finalBalance1 <- getBalance (KeyHashObj kh1)
    finalBalance2 <- getBalance (KeyHashObj kh2)
    finalBalanceD <- getBalance (KeyHashObj kh3)

    finalBalance1 `shouldBe` reward1 <+> depositAmount
    finalBalance2 `shouldBe` mempty
    finalBalanceD `shouldBe` (reward3 <-> partialWithdrawal)

  it "Withdrawals from an unregistered staking address" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2

    account1 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    account2 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountX <- Coin . getPositive <$> arbitrary
    let
      txBody :: forall l. Typeable l => TxBody l era
      txBody =
        mkBasicTxBody
          & withdrawalsTxBodyL .~ Withdrawals [(account1, amountX), (account2, zero)]
    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . WithdrawalAccountsMissing @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . WithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      ]

    account3 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountY <- Coin . getPositive <$> arbitrary
    let subTxOnlyWithdrawal =
          mkBasicTx $
            mkBasicTxBody
              & withdrawalsTxBodyL .~ Withdrawals [(account3, amountY)]
    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody, subTxOnlyWithdrawal])
      [ injectFailure . WithdrawalAccountsMissing @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . WithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . SubWithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . SubWithdrawalAccountsMissing @era $
          Withdrawals [(account1, amountX), (account2, zero)]
      , injectFailure . SubWithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(account3, amountY)]
      , injectFailure . SubWithdrawalAccountsMissing @era $
          Withdrawals [(account3, amountY)]
      ]

  it "Direct deposits to an unregistered account" $ do
    account <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountX <- Coin . getPositive <$> arbitrary
    let
    let
      txBody :: forall l. Typeable l => TxBody l era
      txBody = mkBasicTxBody & directDepositsTxBodyL .~ DirectDeposits [(account, amountX)]
    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . DirectDepositAccountsMissing @era $
          DirectDeposits [(account, amountX)]
      ]

    account2 <- freshKeyHash >>= getAccountAddressFor . KeyHashObj
    amountY <- Coin . getPositive <$> arbitrary
    amountZ <- Coin . getPositive <$> arbitrary
    let
    let subTxOnlyDirectDeposit =
          mkBasicTx $
            mkBasicTxBody & directDepositsTxBodyL .~ DirectDeposits [(account, amountY), (account2, amountZ)]
    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody, subTxOnlyDirectDeposit])
      [ injectFailure . DirectDepositAccountsMissing @era $
          DirectDeposits [(account, amountX)]
      , injectFailure . SubDirectDepositAccountsMissing @era $
          DirectDeposits [(account, amountX)]
      , injectFailure . SubDirectDepositAccountsMissing @era $
          DirectDeposits [(account, amountY), (account2, amountZ)]
      ]

  it "Withdrawals and direct deposits with wrong network id" $ do
    stakeKey <- freshKeyHash
    accountAddress <- registerStakeCredential (KeyHashObj stakeKey)
    let wrongNetworkAccount = accountAddress & accountAddressNetworkIdL .~ Mainnet
    let dd = DirectDeposits [(wrongNetworkAccount, Coin 50)]
    let txBody :: forall l. Typeable l => TxBody l era
        txBody =
          mkBasicTxBody
            & withdrawalsTxBodyL
              .~ Withdrawals [(wrongNetworkAccount, mempty)]
            & directDepositsTxBodyL .~ dd

    submitFailingTx
      (mkBasicTx txBody)
      [ injectFailure . WithdrawalAddressesWithWrongNetwork @era Testnet $ NES.singleton wrongNetworkAccount
      , injectFailure . DirectDepositAddressesWithWrongNetwork @era Testnet $
          NES.singleton wrongNetworkAccount
      , injectFailure . WithdrawalAccountsMissing @era $ Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . WithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . DirectDepositAccountsMissing @era $ dd
      ]

    submitFailingTx
      (mkBasicTx $ txBody & subTransactionsTxBodyL .~ [mkBasicTx txBody])
      [ injectFailure . WithdrawalAddressesWithWrongNetwork @era Testnet $
          NES.singleton wrongNetworkAccount
      , injectFailure . DirectDepositAddressesWithWrongNetwork @era Testnet $
          NES.singleton wrongNetworkAccount
      , injectFailure . WithdrawalAccountsMissing @era $
          Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . WithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . DirectDepositAccountsMissing @era $ dd
      , injectFailure . SubWithdrawalAddressesWithWrongNetwork @era Testnet $
          NES.singleton wrongNetworkAccount
      , injectFailure . SubDirectDepositAddressesWithWrongNetwork @era Testnet $
          NES.singleton wrongNetworkAccount
      , injectFailure . SubWithdrawalAccountsMissingPreBatch @era $
          Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . SubWithdrawalAccountsMissing @era $
          Withdrawals [(wrongNetworkAccount, mempty)]
      , injectFailure . SubDirectDepositAccountsMissing @era $ dd
      ]
  it "Aggregate of top and sub withdrawals exceeds account balance" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (account, reward, _) <- setupAccountAddress
    let subAmount = reward <-> Coin 1
    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, reward)])
            [Withdrawals [(account, subAmount)]]
    submitFailingTx
      tx
      [ injectFailure $
          WithdrawalAmountsExceedingOriginalBalance @era $
            fromJust $
              NEM.fromMap [(account, Mismatch (reward <+> subAmount) reward)]
      ]
    -- legacy mode
    legacyTx <- switchTxToLegacyMode tx
    submitFailingTx
      legacyTx
      [ injectFailure . WithdrawalAmountsInexactInLegacyMode @era $
          NEM.singleton account $
            Mismatch reward (reward <-> subAmount)
      ]

  it "Underflow of applied withdrawal amount is observable in legacy mode" $ do
    modifyPParams $ ppGovActionLifetimeL .~ EpochInterval 2
    (account, reward, _) <- setupAccountAddress

    let moreThanReward = reward <+> Coin 1
    let tx =
          mkTxWithBatchWithdrawals
            (Withdrawals [(account, reward)])
            [Withdrawals [(account, moreThanReward)]]
    submitFailingTx
      tx
      [ injectFailure $
          WithdrawalAmountsExceedingOriginalBalance @era $
            fromJust $
              NEM.fromMap [(account, Mismatch (reward <+> moreThanReward) reward)]
      ]
    legacyTx <- switchTxToLegacyMode tx
    let underflowedBalance =
          -- 18446744073709551615
          Coin . toInteger $ (fromInteger (unCoin reward) :: Word64) - fromInteger (unCoin moreThanReward)
    submitFailingTx
      legacyTx
      [ injectFailure . WithdrawalAmountsInexactInLegacyMode @era $
          NEM.singleton account $
            Mismatch reward underflowedBalance
      , injectFailure $
          WithdrawalAmountsExceedingOriginalBalance @era $
            fromJust $
              NEM.fromMap [(account, Mismatch moreThanReward reward)]
      ]

  describe "Account balance intervals" $ do
    it "Account balance intervals for the top-level transaction" $
      accountBalanceIntervalCases
        (submitFailingTopTxBody . (accountBalanceIntervalsTxBodyL .~))
        WrongNetworkInAccountBalanceIntervals
        MissingAccountsInAccountBalanceIntervals
        BalancesOutsideAccountBalanceIntervals

    it "Account balance intervals within each sub-transaction" $
      accountBalanceIntervalCases
        (submitFailingSubTxBody . (accountBalanceIntervalsTxBodyL .~))
        SubWrongNetworkInAccountBalanceIntervals
        SubMissingAccountsInAccountBalanceIntervals
        SubBalancesOutsideAccountBalanceIntervals

    it "Starting account balance intervals for the top-level transaction" $
      accountBalanceIntervalCases
        (submitFailingTopTxBody . (startingAccountBalanceIntervalsTxBodyL .~))
        WrongNetworkInStartingAccountBalanceIntervals
        MissingAccountsInStartingAccountBalanceIntervals
        BalancesOutsideStartingAccountBalanceIntervals

    it "Satisfied intervals are accepted at every level" $ do
      (accountAddr, balance, _) <- setupAccountAddress
      let intervals = AccountBalanceIntervals [(accountAddr, AccountBalanceExact balance)]
      submitTx_ $ mkBasicTx $ mkBasicTxBody & accountBalanceIntervalsTxBodyL .~ intervals
      submitTx_ $ mkBasicTx $ mkBasicTxBody & startingAccountBalanceIntervalsTxBodyL .~ intervals
      submitTx_ $
        mkBasicTx $
          mkBasicTxBody
            & subTransactionsTxBodyL
              .~ [mkBasicTx $ mkBasicTxBody & accountBalanceIntervalsTxBodyL .~ intervals]

    it "Every violating entry of a single interval map is reported" $ do
      (accountAddr, balance, _) <- setupAccountAddress
      unregistered <- unregisteredAccount
      let onWrongNetwork = accountAddr & accountAddressNetworkIdL .~ Mainnet
          violated = AccountBalanceExact (balance <+> Coin 1)
      submitFailingTx
        ( mkBasicTx $
            mkBasicTxBody
              & accountBalanceIntervalsTxBodyL
                .~ AccountBalanceIntervals
                  [ (onWrongNetwork, violated)
                  , (unregistered, violated)
                  , (accountAddr, violated)
                  ]
        )
        [ injectFailure $
            WrongNetworkInAccountBalanceIntervals @era Testnet (NES.singleton onWrongNetwork)
        , injectFailure $
            MissingAccountsInAccountBalanceIntervals @era (NEM.singleton unregistered violated)
        , injectFailure $
            BalancesOutsideAccountBalanceIntervals @era
              (NEM.singleton accountAddr (balance, violated))
        ]

    it
      "Intervals are checked before the withdrawals and direct deposits of their own transaction are applied"
      $ do
        (accountAddr, balance, _) <- setupAccountAddress
        let deposit = Coin 1_000_000
            txWith modifyBody interval =
              mkBasicTx $
                mkBasicTxBody
                  & accountBalanceIntervalsTxBodyL .~ AccountBalanceIntervals [(accountAddr, interval)]
                  & modifyBody
            drains = withdrawalsTxBodyL .~ Withdrawals [(accountAddr, balance)]
            deposits = directDepositsTxBodyL .~ DirectDeposits [(accountAddr, deposit)]
            expectOutside modifyBody interval =
              submitFailingTx
                (txWith modifyBody interval)
                [ injectFailure $
                    BalancesOutsideAccountBalanceIntervals @era
                      (NEM.singleton accountAddr (balance, interval))
                ]
        expectOutside drains (AccountBalanceExact zero)
        expectOutside deposits (AccountBalanceExact (balance <+> deposit))
        submitFailingTx
          ( mkBasicTx $
              mkBasicTxBody
                & subTransactionsTxBodyL
                  .~ [ mkBasicTx $
                         mkBasicTxBody
                           & withdrawalsTxBodyL .~ Withdrawals [(accountAddr, balance)]
                           & accountBalanceIntervalsTxBodyL
                             .~ AccountBalanceIntervals [(accountAddr, AccountBalanceExact zero)]
                     ]
          )
          [ injectFailure $
              SubBalancesOutsideAccountBalanceIntervals @era
                (NEM.singleton accountAddr (balance, AccountBalanceExact zero))
          ]
        submitTx_ $ txWith drains (AccountBalanceExact balance)

    it "Interval bounds are checked at their boundaries" $ do
      (accountAddr, balance, _) <- setupAccountAddress
      let withInterval interval =
            mkBasicTx $
              mkBasicTxBody
                & accountBalanceIntervalsTxBodyL .~ AccountBalanceIntervals [(accountAddr, interval)]
          intervalHolds = submitTx_ . withInterval
          intervalViolated interval =
            submitFailingTx
              (withInterval interval)
              [ injectFailure $
                  BalancesOutsideAccountBalanceIntervals @era
                    (NEM.singleton accountAddr (balance, interval))
              ]
      intervalHolds $ AccountBalanceExact balance
      intervalViolated $ AccountBalanceExact (balance <+> Coin 1)
      intervalHolds $ AccountBalanceLowerBound (Inclusive balance)
      intervalViolated $ AccountBalanceLowerBound (Inclusive (balance <+> Coin 1))
      intervalHolds $ AccountBalanceUpperBound (Exclusive (balance <+> Coin 1))
      intervalViolated $ AccountBalanceUpperBound (Exclusive balance)
      intervalHolds $ AccountBalanceBothBounds (Inclusive balance) (Exclusive (balance <+> Coin 1))
      intervalViolated $
        AccountBalanceBothBounds (Inclusive (balance <+> Coin 1)) (Exclusive (balance <+> Coin 2))

    it
      "Starting intervals see the pre-transaction balance, account balance intervals see the post-sub-transaction balance"
      $ do
        (accountAddr, balance, _) <- setupAccountAddress
        let drainingSubTx :: Tx SubTx era
            drainingSubTx =
              mkBasicTx $ mkBasicTxBody & withdrawalsTxBodyL .~ Withdrawals [(accountAddr, balance)]
            txWithIntervals starting current =
              mkBasicTx $
                mkBasicTxBody
                  & subTransactionsTxBodyL .~ [drainingSubTx]
                  & startingAccountBalanceIntervalsTxBodyL
                    .~ AccountBalanceIntervals [(accountAddr, starting)]
                  & accountBalanceIntervalsTxBodyL
                    .~ AccountBalanceIntervals [(accountAddr, current)]
            original = AccountBalanceExact balance
            drained = AccountBalanceExact zero
        submitFailingTx
          (txWithIntervals drained original)
          [ injectFailure $
              BalancesOutsideAccountBalanceIntervals @era (NEM.singleton accountAddr (zero, original))
          , injectFailure $
              BalancesOutsideStartingAccountBalanceIntervals @era
                (NEM.singleton accountAddr (balance, drained))
          ]
        submitTx_ $ txWithIntervals original drained
  where
    setupAccountAddress :: ImpTestM era (AccountAddress, Coin, KeyHash Staking)
    setupAccountAddress = do
      kh <- freshKeyHash
      let cred = KeyHashObj kh
      ra <- registerStakeCredential cred
      submitAndExpireProposalToMakeReward cred
      b <- getBalance cred
      pure (ra, b, kh)

    mkTxWithBatchWithdrawals :: Withdrawals -> [Withdrawals] -> Tx TopTx era
    mkTxWithBatchWithdrawals topWdrls subs =
      mkBasicTx $
        mkBasicTxBody
          & withdrawalsTxBodyL .~ topWdrls
          & subTransactionsTxBodyL .~ OMap.fromFoldable (fmap mkSubTx subs)
      where
        mkSubTx :: Withdrawals -> Tx SubTx era
        mkSubTx w = mkBasicTx (mkBasicTxBody & withdrawalsTxBodyL .~ w)

    unregisteredAccount :: ImpTestM era AccountAddress
    unregisteredAccount = freshKeyHash >>= getAccountAddressFor . KeyHashObj

    submitFailingTopTxBody modifyBody failure =
      submitFailingTx (mkBasicTx (mkBasicTxBody & modifyBody)) [injectFailure failure]

    submitFailingSubTxBody modifyBody failure =
      submitFailingTx
        (mkBasicTx (mkBasicTxBody & subTransactionsTxBodyL .~ [mkBasicTx (mkBasicTxBody & modifyBody)]))
        [injectFailure failure]

    accountBalanceIntervalCases ::
      (AccountBalanceIntervals era -> t era -> ImpTestM era ()) ->
      (Network -> NES.NonEmptySet AccountAddress -> t era) ->
      (NEM.NonEmptyMap AccountAddress (AccountBalanceInterval era) -> t era) ->
      (NEM.NonEmptyMap AccountAddress (Coin, AccountBalanceInterval era) -> t era) ->
      ImpTestM era ()
    accountBalanceIntervalCases submitFailing mkWrongNetwork mkMissingAccounts mkBalancesOutside = do
      (accountAddr, balance, _) <- setupAccountAddress
      let onWrongNetwork = accountAddr & accountAddressNetworkIdL .~ Mainnet
          violated = AccountBalanceExact (balance <+> Coin 1)
      submitFailing
        (AccountBalanceIntervals [(onWrongNetwork, violated)])
        (mkWrongNetwork Testnet (NES.singleton onWrongNetwork))
      unregistered <- unregisteredAccount
      submitFailing
        (AccountBalanceIntervals [(unregistered, violated)])
        (mkMissingAccounts (NEM.singleton unregistered violated))
      submitFailing
        (AccountBalanceIntervals [(accountAddr, violated)])
        (mkBalancesOutside (NEM.singleton accountAddr (balance, violated)))
