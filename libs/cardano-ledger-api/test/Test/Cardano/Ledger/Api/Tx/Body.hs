{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Tx.Body (spec) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.State hiding (consumed)
import Cardano.Ledger.Val
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.MapExtras as Map (extract)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Api.Arbitrary ()
import Test.Cardano.Ledger.Common

totalTxDeposits ::
  (EraTxBody era, EraCertState era) =>
  Network ->
  PParams era ->
  CertState era ->
  TxBody l era ->
  Coin
totalTxDeposits network pp dpstate txb =
  numKeys <×> pp ^. ppKeyDepositL <+> snd (foldl' accum (regpools, Coin 0) certs)
  where
    certs = toList (txb ^. certsTxBodyL)
    numKeys = length $ filter isRegStakeTxCert certs
    regpools =
      Map.mapWithKey (`stakePoolStateToStakePoolParams` network) $ psStakePools (dpstate ^. certPStateL)
    accum (!pools, !ans) (RegPoolTxCert stakePoolParams) =
      -- We don't pay a deposit on a pool that is already registered
      if Map.member (sppId stakePoolParams) pools
        then (pools, ans)
        else (Map.insert (sppId stakePoolParams) stakePoolParams pools, ans <+> pp ^. ppPoolDepositL)
    accum ans _ = ans

keyTxRefunds ::
  (EraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  PParams era ->
  CertState era ->
  TxBody l era ->
  Coin
keyTxRefunds pp dpstate tx =
  case foldl' accum (initAccountsMap, Set.empty, mempty) certs of
    (_, _, res) -> res
  where
    certs = tx ^. certsTxBodyL
    initAccountsMap = dpstate ^. certDStateL . accountsL . accountsMapL
    keyDeposit = pp ^. ppKeyDepositL
    accum acc@(!accountsMap, !newlyRegistered, !ans) = \case
      RegTxCert cred
        | Map.member cred accountsMap || Set.member cred newlyRegistered -> acc
        | otherwise -> (accountsMap, Set.insert cred newlyRegistered, ans)
      UnRegTxCert cred ->
        case Map.extract cred accountsMap of
          (Just accountState, newAccountsMap) ->
            (newAccountsMap, newlyRegistered, ans <> fromCompact (accountState ^. depositAccountStateL))
          (Nothing, newAccountsMap)
            | Set.member cred newlyRegistered ->
                (newAccountsMap, Set.delete cred newlyRegistered, ans <> keyDeposit)
            | otherwise -> acc
      _ -> acc

-- | This is the old implementation of `evalBodyTxBody`. We keep it around to ensure that
-- the produced result hasn't changed
evaluateTransactionBalance ::
  (MaryEraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  Network ->
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody TopTx era ->
  Value era
evaluateTransactionBalance network pp dpstate utxo txBody =
  evaluateTransactionBalanceShelley network pp dpstate utxo txBody <> (txBody ^. mintValueTxBodyF)

evaluateTransactionBalanceShelley ::
  (EraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  Network ->
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody TopTx era ->
  Value era
evaluateTransactionBalanceShelley network pp dpstate utxo txBody = consumed <-> produced
  where
    produced =
      sumUTxO (txouts txBody)
        <+> inject (txBody ^. feeTxBodyL <+> totalTxDeposits network pp dpstate txBody)
    consumed =
      sumUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
        <> inject (refunds <> withdrawals)
    refunds = keyTxRefunds pp dpstate txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

-- | Randomly lookup pool params and staking credentials to add them as unregistration and
-- undelegation certificates respectively.
genTxBodyFrom ::
  (EraTxBody era, ShelleyEraTxCert era, Arbitrary (TxBody l era), EraCertState era) =>
  CertState era ->
  UTxO era ->
  Gen (TxBody l era)
genTxBodyFrom certState (UTxO u) = do
  txBody <- arbitrary
  inputs <- sublistOf (Map.keys u)
  unDelegCreds <- sublistOf (Map.keys (certState ^. certDStateL . accountsL . accountsMapL))
  deRegKeys <- sublistOf (Map.keys (certState ^. certPStateL . psStakePoolsL))
  network <- arbitrary
  let deReg =
        Map.elems $
          Map.mapWithKey (`stakePoolStateToStakePoolParams` network) $
            Map.restrictKeys (certState ^. certPStateL . psStakePoolsL) (Set.fromList deRegKeys)
  certs <-
    shuffle $
      toList (txBody ^. certsTxBodyL)
        <> (UnRegTxCert <$> unDelegCreds)
        <> (RegPoolTxCert <$> deReg)
  pure
    ( txBody
        & inputsTxBodyL .~ Set.fromList inputs
        & certsTxBodyL .~ SSeq.fromList certs
    )

propEvalBalanceTxBody ::
  ( EraUTxO era
  , MaryEraTxBody era
  , ShelleyEraTxCert era
  , Arbitrary (TxBody TopTx era)
  , EraCertState era
  ) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceTxBody pp certState utxo = do
  property $
    forAll (genTxBodyFrom @_ @TopTx certState utxo) $ \txBody ->
      forAll arbitrary $ \network ->
        evalBalanceTxBody pp lookupKeyDeposit (const Nothing) isRegPoolId utxo txBody
          `shouldBe` evaluateTransactionBalance network pp certState utxo txBody
  where
    lookupKeyDeposit = lookupDepositDState (certState ^. certDStateL)
    isRegPoolId = (`Map.member` psStakePools (certState ^. certPStateL))

propEvalBalanceShelleyTxBody ::
  (EraUTxO era, ShelleyEraTxCert era, Arbitrary (TxBody TopTx era), EraCertState era) =>
  Network ->
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceShelleyTxBody network pp certState utxo =
  property $
    forAll (genTxBodyFrom @_ @TopTx certState utxo) $ \txBody ->
      evalBalanceTxBody pp lookupKeyDeposit (const Nothing) isRegPoolId utxo txBody
        `shouldBe` evaluateTransactionBalanceShelley network pp certState utxo txBody
  where
    lookupKeyDeposit = lookupDepositDState (certState ^. certDStateL)
    isRegPoolId = (`Map.member` psStakePools (certState ^. certPStateL))

-- | NOTE: We cannot have this property pass for Conway and beyond because Conway changes this calculation.
-- This property test only exists to confirm that the old and new implementations for the evalBalanceTxBody` API matched,
-- and this can be ascertained only until Babbage.
spec :: Spec
spec =
  describe "TxBody" $ do
    describe "ShelleyEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceShelleyTxBody @ShelleyEra
    describe "AllegraEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceShelleyTxBody @AllegraEra
    describe "MaryEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @MaryEra
    describe "AlonzoEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @AlonzoEra
    describe "BabbageEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @BabbageEra
