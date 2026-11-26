{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Address (
  AccountAddress,
  DirectDeposits (..),
  Withdrawals (..),
  accountAddressCredentialL,
 )
import Cardano.Ledger.Allegra.Scripts (invalidBeforeL)
import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  Exclusive (..),
  Inclusive (..),
  StrictMaybe (..),
  addEpochInterval,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.TxCert (Delegatee (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Dijkstra.Core (
  AllegraEraTxBody (..),
  AlonzoEraTxWits (..),
  DijkstraEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  InjectRuleFailure (..),
  MaryEraTxBody (..),
  Tx,
  TxLevel (..),
  ppKeyDepositL,
  ppMaxTxExUnitsL,
  pattern DelegTxCert,
  pattern UnRegDepositTxCert,
 )
import Cardano.Ledger.Dijkstra.Rules (DijkstraUtxoPredFailure (..), EntitiesPredFailure (..))
import Cardano.Ledger.Dijkstra.Scripts (AccountBalanceInterval (..), AccountBalanceIntervals (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Mary.Value (AssetName (..), MaryValue (..), MultiAsset (..), PolicyID (..))
import Cardano.Ledger.Plutus (
  ExUnits (..),
  Language (..),
  PlutusLanguage,
  SLanguage,
  hashPlutusScript,
  withSLanguage,
 )
import Cardano.Ledger.Plutus.TxInfo (transKeyHash)
import Cardano.Ledger.Shelley.LedgerState (nesELL)
import Cardano.Ledger.Slot (SlotNo)
import Cardano.Ledger.Tools (ensureMinCoinTxOut)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val ((<->))
import Control.Monad ((>=>))
import Control.Monad.State (gets)
import Data.Foldable (fold)
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.Common as P
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Common (SpecWith, describe, forM_, it)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  ImpInit,
  ImpTestM,
  LedgerSpec,
  fixupPPHash,
  freshKeyAddrNoPtr_,
  freshKeyHash,
  getAccountBalance,
  getsNES,
  getsPParams,
  impAddPlutusScript,
  impCurSlotNoG,
  impEpochFirstSlot,
  impGetUTxO,
  impSlotToPOSIXTime,
  passNEpochs,
  rederiveAddrTxWits,
  registerPool,
  registerStakeCredential,
  submitFailingTx,
  submitPhase2Invalid_,
  submitTxAnn,
  submitTxAnn_,
  withPostFixup,
 )
import Test.Cardano.Ledger.Imp.Common (choose, shouldReturn)
import Test.Cardano.Ledger.Plutus (PlutusArgs (..), applyPlutusParams)
import Test.Cardano.Ledger.Plutus.Examples (
  alwaysSucceedsNoDatum,
  inputSpendingGuard,
  partialWithdrawalVestingScript,
  purposeIsWellformedNoDatum,
 )

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "Plutus" $ do
    forM_ [l | l <- eraLanguages @era, l >= PlutusV4] $ \lang ->
      withSLanguage lang $ \slang -> do
        describe "purposeIsWellformedNoDatum" $ do
          it "Passes with guarding purpose" $ do
            let sh = hashPlutusScript $ purposeIsWellformedNoDatum slang
            submitTxAnn_ "Submit tx with a Plutus guard" $
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ OSet.singleton (ScriptHashObj sh)
        -- Pre-Dijkstra, transactions which need to perform translation level
        -- validation (such as batcher transactions which process multiple order
        -- utxos in a single transaction) would use the withdraw-zero trick to
        -- perform tx level validation in order to save ExBudget.
        -- Dijkstra onwards, guard scripts can perform tx-level validation.
        -- This is better than the withdraw-zero trick because we don't need to
        -- perform stake credential registration and provide a deposit.
        --
        -- This scenario submits a tx which spends multiple script utxos that
        -- contain a specific protocol token. The script locking those utxos is
        -- also their guard: when spending, it only forwards validation to its
        -- guard execution, which verifies that each script utxo has the
        -- specific protocol token.

        -- This scenario shows the delegation mechanism, not a secure tx
        -- batcher. For example, a real batcher guard would also check that each
        -- order's owner is paid, in one pass over the inputs and outputs
        -- instead of one per spending script (which is why the withdraw-zero
        -- trick was used).
        describe "inputSpendingGuard" $ do
          let inputSpendingGuardScriptHash = hashPlutusScript $ inputSpendingGuard slang

              submitOrders :: Int -> Int -> ImpTestM era [TxIn]
              submitOrders nbOrdersWithToken nbOrdersWithoutToken = do
                let
                  protocolPolicyId = PolicyID $ hashPlutusScript $ alwaysSucceedsNoDatum slang
                  mkProtocolTokenMultiAsset = MultiAsset . Map.singleton protocolPolicyId . Map.singleton (AssetName "protocol")
                  mkOrderTxOut = mkBasicTxOut (mkAddr inputSpendingGuardScriptHash StakeRefNull) . MaryValue mempty
                  orderTxOuts =
                    replicate nbOrdersWithToken (mkOrderTxOut $ mkProtocolTokenMultiAsset 1)
                      ++ replicate nbOrdersWithoutToken (mkOrderTxOut mempty)
                tx <-
                  submitTxAnn "Create order UTxOs" $
                    mkBasicTx mkBasicTxBody
                      & bodyTxL . mintTxBodyL .~ mkProtocolTokenMultiAsset (toInteger nbOrdersWithToken)
                      & bodyTxL . outputsTxBodyL .~ SSeq.fromList orderTxOuts
                pure [txInAt i tx | i <- [0 .. length orderTxOuts - 1]]

              mkOrderProcessingTx :: [TxIn] -> ImpTestM era (Tx TopTx era)
              mkOrderProcessingTx orderTxIns = do
                orderValues <- mapM (fmap (^. valueTxOutL) . impGetUTxO) orderTxIns
                protocolAddr <- freshKeyAddrNoPtr_
                pp <- getsPParams id
                pure $
                  mkBasicTx mkBasicTxBody
                    & bodyTxL . inputsTxBodyL .~ Set.fromList orderTxIns
                    -- A single order's ADA can be below the min ADA of the protocol address
                    & bodyTxL . outputsTxBodyL .~ [ensureMinCoinTxOut pp . mkBasicTxOut protocolAddr $ fold orderValues]
                    & bodyTxL . guardsTxBodyL .~ OSet.singleton (ScriptHashObj inputSpendingGuardScriptHash)

              -- The ExUnits of a failing script can't be estimated, so the fixup assigns it the whole
              -- `ppMaxTxExUnitsL`, which exceeds the tx limit as soon as any other script runs.
              -- Share the limit equally between all redeemers instead, which is still far more than
              -- these scripts need, so they can only fail on their own logic.
              submitPhase2InvalidSharingExUnits = do
                let
                  shareMaxTxExUnits :: Tx TopTx era -> ImpTestM era (Tx TopTx era)
                  shareMaxTxExUnits tx = do
                    ExUnits mem steps <- getsPParams ppMaxTxExUnitsL
                    let redeemers = tx ^. witsTxL . rdmrsTxWitsL . unRedeemersL
                        n = fromIntegral $ Map.size redeemers
                        sharedExUnits = ExUnits (mem `div` n) (steps `div` n)
                    pure $
                      tx
                        & witsTxL . rdmrsTxWitsL . unRedeemersL
                          .~ fmap (\(dat, _) -> (dat, sharedExUnits)) redeemers
                withPostFixup (shareMaxTxExUnits >=> fixupPPHash >=> rederiveAddrTxWits)
                  . submitPhase2Invalid_
          it "Batches orders validated once by the guard" $ do
            nbOrdersWithToken <- choose (1, 10)
            orderTxIns <- submitOrders nbOrdersWithToken 0
            mkOrderProcessingTx orderTxIns >>= submitTxAnn_ "Submit batch tx with the guard inputSpendingGuard"
          it "Fails when the guard is missing" $ do
            nbOrdersWithToken <- choose (1, 10)
            orderTxIns <- submitOrders nbOrdersWithToken 0
            orderProcessingTx <- mkOrderProcessingTx orderTxIns
            submitPhase2InvalidSharingExUnits $ orderProcessingTx & bodyTxL . guardsTxBodyL .~ mempty
          it "Fails when an order lacks the protocol token" $ do
            nbOrdersWithToken <- choose (1, 10)
            nbOrdersWithNoToken <- choose (1, 3)
            orderTxIns <- submitOrders nbOrdersWithToken nbOrdersWithNoToken
            mkOrderProcessingTx orderTxIns >>= submitPhase2InvalidSharingExUnits

        -- Scenario: an employer (or a project paying a grant) vests 12K ADA to
        -- an employee linearly over a year. The employee may claim whatever has
        -- vested, whenever they want.

        -- Pre-Dijkstra, vesting needs a state UTxO with a datum, and every
        -- claim spends it and recreates it with a continuing output which
        -- brings a set of issues such as utxo contention, min-ADA and datum
        -- bookkeeping. With Dijkstra, the vesting lives in a reward account and
        -- needs no state at all.
        --
        -- This scenario showcases:
        --
        -- \* Direct deposists in reward accounts
        -- \* Partial withdrawals
        -- \* Reward account balance interval checks
        --
        -- Implementation details:
        --
        -- \* Vesting account: a reward account whose credential is a withdrawing script parameterized by
        --   `{beneficiary, total, start, end}`. It must be registered before it can receive deposits.
        -- \* Funding: the employer direct-deposits `total` into the account.
        -- \* Claim: the beneficiary withdraws any amount `w` (partial withdrawal, outside legacy mode), as
        --   long as the remaining balance stays at or above
        --   `locked(t) = total × (end − t) / (end − start)`, clamped to `[0, total]` and rounded up.
        --   The script never tracks past claims: they are already reflected in the balance, so
        --   claimable is always `balance − locked(t)`. The withdrawing script checks:
        --   * the beneficiary signed the tx;
        --   * `t` is the lower bound of the tx's validity range, which must be finite. The ledger
        --     requires the current slot to be at or after it, so the beneficiary can only pick an
        --     earlier `t`, which locks more;
        --   * `txInfoAccountBalanceIntervals` has a lower bound `lo` for its own account with
        --     `lo ≥ locked(t) + w`, where `w` is its own entry in `txInfoWithdrawals`. The ledger checks
        --     the interval against the balance *before* this tx's withdrawals are applied, so this
        --     guarantees `balance − w ≥ lo − w ≥ locked(t)`.
        -- \* Bonus: reward account balances count towards the credential's stake, so if the account is
        --   delegated, the locked funds keep earning staking rewards. Those, and any extra direct
        --   deposits, are claimable immediately since only the `locked(t)` floor is enforced.
        -- \* Negative cases:
        --   * over-claiming by 1 Lovelace with the actual balance as `lo` (`lo < locked(t) + w`): script fails;
        --   * over-claiming with `lo` above the actual balance (`lo > balance`): `BalancesOutsideAccountBalanceIntervals`;
        --   * no interval for its own account: script fails;
        --   * an upper-bound-only interval for its own account: script fails;
        --   * validity range with no lower bound: script fails;
        --   * validity range starting in the future, to claim at a later `t`: `OutsideValidityIntervalUTxO`;
        --   * signed by someone other than the beneficiary: script fails;
        --   * delegating the account without the beneficiary's signature: script fails;
        --   * unregistering the account: script fails. The registration deposit is therefore locked
        --     forever, even for the beneficiary.
        statelessVestingAccountSpec slang

data VestingAccount = VestingAccount
  { vaBeneficiary :: KeyHash Guard
  , vaAccountAddress :: AccountAddress
  , vaTotalAmount :: Coin
  , vaStartTime :: Integer
  -- ^ POSIX time in milliseconds
  , vaEndTime :: Integer
  -- ^ POSIX time in milliseconds
  }

statelessVestingAccountSpec ::
  forall era l.
  (DijkstraEraImp era, PlutusLanguage l) =>
  SLanguage l ->
  SpecWith (ImpInit (LedgerSpec era))
statelessVestingAccountSpec slang = do
  describe "statelessVestingAccount" $ do
    describe "nominal scenario" $ do
      it "Beneficiary claims the vested funds with partial withdrawals" $ do
        va <- setupVestingAccount

        -- No time has passed since setting up vesting account, so withdraw
        -- should not be possible.
        slot <- gets (^. impCurSlotNoG)
        claimableAmountAt va slot `shouldReturn` mempty

        passNEpochs 1 >> claimFullWithdrawableAmount va
        passNEpochs 2 >> claimFullWithdrawableAmount va
        -- Past the end of the vesting schedule, everything left is claimable
        passNEpochs 2 >> claimFullWithdrawableAmount va
        getAccountBalance (vaAccountAddress va) `shouldReturn` mempty
      it "Beneficiary claims everything at once after the vesting ends" $ do
        va <- setupVestingAccount
        passNEpochs 4
        slot <- gets (^. impCurSlotNoG)
        claimableAmountAt va slot `shouldReturn` vaTotalAmount va
        claimFullWithdrawableAmount va
        getAccountBalance (vaAccountAddress va) `shouldReturn` mempty
      it "Beneficiary claims part of the vested funds and the rest later" $ do
        va <- setupVestingAccount
        passNEpochs 2
        slotNo <- gets (^. impCurSlotNoG)
        Coin claimableAmount <- claimableAmountAt va slotNo
        partialClaim <- Coin <$> choose (1, claimableAmount - 1)
        claim va slotNo partialClaim
        claimableAmountAt va slotNo `shouldReturn` (Coin claimableAmount <-> partialClaim)
        claimFullWithdrawableAmount va
        claimableAmountAt va slotNo `shouldReturn` mempty
      -- The extra deposit also simulates staking rewards paid into the vesting account: the
      -- script only enforces that the user can't withdraw more than the allowed
      -- vested amount.
      it "Extra direct deposits are claimable immediately" $ do
        va <- setupVestingAccount
        extraDeposit <- Coin <$> choose (1_000_000, 100_000_000)
        submitTxAnn_ "Employer deposits a bonus into the vesting account" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . directDepositsTxBodyL .~ DirectDeposits [(vaAccountAddress va, extraDeposit)]
        -- Nothing has vested yet, so only the bonus is claimable
        slotNo <- gets (^. impCurSlotNoG)
        claimableAmountAt va slotNo `shouldReturn` extraDeposit
        claimFullWithdrawableAmount va
        getAccountBalance (vaAccountAddress va) `shouldReturn` vaTotalAmount va
      it "Beneficiary claims at an earlier time (i.e. earlier validity interval lower bound)" $ do
        va <- setupVestingAccount
        passNEpochs 1
        earlierSlot <- gets (^. impCurSlotNoG)
        passNEpochs 1
        -- The validity range starts before the current slot, so the script sees an earlier time
        claimableAmountAt va earlierSlot >>= claim va earlierSlot
        -- What vested in between is still claimable
        claimFullWithdrawableAmount va
      it "Beneficiary claims with any account balance interval that has a lower bound" $ do
        va <- setupVestingAccount
        let claimFullWithdrawableAmountWith mkInterval = do
              slotNo <- gets (^. impCurSlotNoG)
              claimable <- claimableAmountAt va slotNo
              claimWith mkInterval va slotNo claimable
        passNEpochs 1
        claimFullWithdrawableAmountWith $ \balance ->
          AccountBalanceBothBounds (Inclusive balance) (Exclusive $ balance <> Coin 1)
        passNEpochs 1
        claimFullWithdrawableAmountWith AccountBalanceExact
    describe "failure scenario" $ do
      -- Over-claiming by 1 Lovelace with the actual balance as the account balance lower bound: script fails
      it "over-claiming with the actual balance as lower bound" $ do
        va <- setupVestingAccount
        passNEpochs 1
        slot <- gets (^. impCurSlotNoG)
        claimable <- claimableAmountAt va slot
        balance <- getAccountBalance (vaAccountAddress va)
        submitPhase2Invalid_ $
          mkClaimTx va slot (claimable <> Coin 1) (AccountBalanceLowerBound (Inclusive balance))
      -- Over-claiming with a account balance lower bound above the actual balance: `BalancesOutsideAccountBalanceIntervals`
      it "over-claiming with a lower bound above the actual balance" $ do
        va <- setupVestingAccount
        passNEpochs 1
        slot <- gets (^. impCurSlotNoG)
        claimable <- claimableAmountAt va slot
        balance <- getAccountBalance (vaAccountAddress va)
        -- Inflating `lo` by the over-claimed amount satisfies the script, so only the ledger stops it
        let inflatedInterval = AccountBalanceLowerBound (Inclusive $ balance <> Coin 1)
        submitFailingTx
          (mkClaimTx va slot (claimable <> Coin 1) inflatedInterval)
          [ injectFailure $
              BalancesOutsideAccountBalanceIntervals @era
                (NEM.singleton (vaAccountAddress va) (balance, inflatedInterval))
          ]
      -- No account balance interval specified: script fails;
      it "no account balance interval specified in the tx" $ do
        va <- setupVestingAccount
        passNEpochs 1
        claimTx <- mkFullClaimTx va
        submitPhase2Invalid_ $
          claimTx & bodyTxL . accountBalanceIntervalsTxBodyL .~ AccountBalanceIntervals []
      -- Only specify upper bound in account balance interval: script fails;
      it "only specify upper bound in account balance interval" $ do
        va <- setupVestingAccount
        passNEpochs 1
        claimTx <- mkFullClaimTx va
        balance <- getAccountBalance (vaAccountAddress va)
        submitPhase2Invalid_ $
          claimTx
            & bodyTxL . accountBalanceIntervalsTxBodyL
              .~ AccountBalanceIntervals
                [(vaAccountAddress va, AccountBalanceUpperBound (Exclusive $ balance <> Coin 1))]
      -- validity range with no lower bound: script fails;
      it "validity range with no lower bound" $ do
        va <- setupVestingAccount
        passNEpochs 1
        claimTx <- mkFullClaimTx va
        submitPhase2Invalid_ $ claimTx & bodyTxL . vldtTxBodyL . invalidBeforeL .~ SNothing
      -- validity range starting in the future, to claim at a later `t`: `OutsideValidityIntervalUTxO`;
      it "validity range starting in the future" $ do
        va <- setupVestingAccount
        currentSlot <- gets (^. impCurSlotNoG)
        currentEpoch <- getsNES nesELL
        -- Pretending that the vesting has already ended would release everything
        vestingEndSlot <- impEpochFirstSlot (addEpochInterval currentEpoch (EpochInterval 4))
        balance <- getAccountBalance (vaAccountAddress va)
        let claimTx = mkClaimTx va vestingEndSlot balance (AccountBalanceLowerBound (Inclusive balance))
        submitFailingTx
          claimTx
          [ injectFailure $
              OutsideValidityIntervalUTxO @era (claimTx ^. bodyTxL . vldtTxBodyL) currentSlot
          ]
      -- signed by someone other than the beneficiary: script fails;
      it "signed by someone other than the beneficiary" $ do
        va <- setupVestingAccount
        passNEpochs 1
        claimTx <- mkFullClaimTx va
        someoneElse <- freshKeyHash
        submitPhase2Invalid_ $
          claimTx & bodyTxL . guardsTxBodyL .~ OSet.singleton (KeyHashObj someoneElse)
      -- delegating the account without the beneficiary's signature: script fails;
      it "delegating the account without the beneficiary's signature" $ do
        va <- setupVestingAccount
        pool <- freshKeyHash
        registerPool pool
        submitPhase2Invalid_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [DelegTxCert (vaAccountAddress va ^. accountAddressCredentialL) (DelegStake pool)]
      -- unregistering the account: script fails.
      it "unregistering the account" $ do
        va <- setupVestingAccount
        -- Only an empty account can be unregistered
        passNEpochs 4
        claimFullWithdrawableAmount va
        keyDeposit <- getsPParams ppKeyDepositL
        submitPhase2Invalid_ $
          mkBasicTx mkBasicTxBody
            & bodyTxL . certsTxBodyL
              .~ [UnRegDepositTxCert (vaAccountAddress va ^. accountAddressCredentialL) keyDeposit]
            & bodyTxL . guardsTxBodyL .~ OSet.singleton (KeyHashObj (vaBeneficiary va))
  where
    -- Vest `vaTotalAmount` linearly over 4 epochs, starting now
    setupVestingAccount :: ImpTestM era VestingAccount
    setupVestingAccount = do
      beneficiary <- freshKeyHash
      let totalVestingAmount = Coin 12_000_000_000
      vestingStartTime <- impSlotToPOSIXTime =<< gets (^. impCurSlotNoG)
      currentEpoch <- getsNES nesELL
      vestingEndTime <-
        impSlotToPOSIXTime =<< impEpochFirstSlot (addEpochInterval currentEpoch (EpochInterval 4))
      vestingScriptHash <-
        impAddPlutusScript
          ( partialWithdrawalVestingScript slang
              `applyPlutusParams` [ PV4.toData $ transKeyHash beneficiary
                                  , PV4.toData $ unCoin totalVestingAmount
                                  , PV4.toData vestingStartTime
                                  , PV4.toData vestingEndTime
                                  ]
          )
          (PlutusArgs (P.Constr 0 []) Nothing)
      vestingAccountAddress <- registerStakeCredential $ ScriptHashObj vestingScriptHash
      submitTxAnn_ "Employer funds the vesting account" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . directDepositsTxBodyL .~ DirectDeposits [(vestingAccountAddress, totalVestingAmount)]
      getAccountBalance vestingAccountAddress `shouldReturn` totalVestingAmount
      pure
        VestingAccount
          { vaBeneficiary = beneficiary
          , vaAccountAddress = vestingAccountAddress
          , vaTotalAmount = totalVestingAmount
          , vaStartTime = vestingStartTime
          , vaEndTime = vestingEndTime
          }

    -- Claims everything that is claimable at the current slot
    claimFullWithdrawableAmount :: VestingAccount -> ImpTestM era ()
    claimFullWithdrawableAmount va = do
      slotNo <- gets (^. impCurSlotNoG)
      claimableAmountAt va slotNo >>= claim va slotNo

    -- Balance of the vesting account above what is still locked at `slot`
    claimableAmountAt :: VestingAccount -> SlotNo -> ImpTestM era Coin
    claimableAmountAt va slot = do
      time <- impSlotToPOSIXTime slot
      balance <- getAccountBalance (vaAccountAddress va)
      pure $ balance <-> lockedVestingAmount va time

    -- Mirrors the vesting script: Lovelace still locked at POSIX time `t`, rounded up
    lockedVestingAmount :: VestingAccount -> Integer -> Coin
    lockedVestingAmount VestingAccount {vaTotalAmount = Coin totalVestingAmount, vaStartTime, vaEndTime} time
      | time <= vaStartTime = Coin totalVestingAmount
      | time >= vaEndTime = Coin 0
      | otherwise = Coin $ (totalVestingAmount * (vaEndTime - time) + duration - 1) `div` duration
      where
        duration = vaEndTime - vaStartTime

    claim :: VestingAccount -> SlotNo -> Coin -> ImpTestM era ()
    claim = claimWith (AccountBalanceLowerBound . Inclusive)

    -- Claims with an account balance interval built from the current balance
    claimWith ::
      (Coin -> AccountBalanceInterval era) -> VestingAccount -> SlotNo -> Coin -> ImpTestM era ()
    claimWith mkInterval va slot claimAmount = do
      accountBalance <- getAccountBalance (vaAccountAddress va)
      let claimTx = mkClaimTx va slot claimAmount (mkInterval accountBalance)
      submitTxAnn_ "Beneficiary claims from the vesting account" claimTx
      getAccountBalance (vaAccountAddress va) `shouldReturn` (accountBalance <-> claimAmount)

    -- A valid claim of everything claimable at the current slot, for failure scenarios to break
    mkFullClaimTx :: VestingAccount -> ImpTestM era (Tx TopTx era)
    mkFullClaimTx va = do
      slotNo <- gets (^. impCurSlotNoG)
      claimableAmount <- claimableAmountAt va slotNo
      accountBalance <- getAccountBalance (vaAccountAddress va)
      pure $ mkClaimTx va slotNo claimableAmount (AccountBalanceLowerBound (Inclusive accountBalance))

    mkClaimTx :: VestingAccount -> SlotNo -> Coin -> AccountBalanceInterval era -> Tx TopTx era
    mkClaimTx VestingAccount {vaAccountAddress, vaBeneficiary} slotNo claimAmount accountBalanceInterval =
      mkBasicTx mkBasicTxBody
        & bodyTxL . withdrawalsTxBodyL .~ Withdrawals [(vaAccountAddress, claimAmount)]
        & bodyTxL . accountBalanceIntervalsTxBodyL
          .~ AccountBalanceIntervals [(vaAccountAddress, accountBalanceInterval)]
        -- The beneficiary signs the transaction through a key hash guard
        & bodyTxL . guardsTxBodyL .~ OSet.singleton (KeyHashObj vaBeneficiary)
        -- The vesting script reads the current time from the lower bound of the validity interval
        & bodyTxL . vldtTxBodyL . invalidBeforeL .~ SJust slotNo
