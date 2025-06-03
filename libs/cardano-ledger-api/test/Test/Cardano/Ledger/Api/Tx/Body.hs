{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Tx.Body (spec) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.UTxO hiding (consumed, produced)
import Cardano.Ledger.State hiding (consumed)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Api.Arbitrary ()
import Test.Cardano.Ledger.Common

totalTxDeposits ::
  (EraTxBody era, EraCertState era) =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
totalTxDeposits pp dpstate txb =
  numKeys <×> pp ^. ppKeyDepositL <+> snd (foldl' accum (regpools, Coin 0) certs)
  where
    certs = toList (txb ^. certsTxBodyL)
    numKeys = length $ filter isRegStakeTxCert certs
    regpools = psStakePoolParams (dpstate ^. certPStateL)
    accum (!pools, !ans) (RegPoolTxCert poolparam) =
      -- We don't pay a deposit on a pool that is already registered
      if Map.member (ppId poolparam) pools
        then (pools, ans)
        else (Map.insert (ppId poolparam) poolparam pools, ans <+> pp ^. ppPoolDepositL)
    accum ans _ = ans

keyTxRefunds ::
  (EraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
keyTxRefunds pp dpstate tx = snd (foldl' accum (initialKeys, Coin 0) certs)
  where
    certs = tx ^. certsTxBodyL
    initialKeys = UM.RewDepUView $ dpstate ^. certDStateL . dsUnifiedL
    keyDeposit = UM.compactCoinOrError (pp ^. ppKeyDepositL)
    accum (!keys, !ans) (RegTxCert k) =
      -- Deposit is added locally to the growing 'keys'
      (UM.RewDepUView $ UM.insert k (UM.RDPair mempty keyDeposit) keys, ans)
    accum (!keys, !ans) (UnRegTxCert k) =
      -- If the key is registered, lookup the deposit in the locally growing 'keys'
      -- if it is not registered, then just return ans
      case UM.lookup k keys of
        Just (UM.RDPair _ deposit) -> (keys, ans <+> fromCompact deposit)
        Nothing -> (keys, ans)
    accum ans _ = ans

-- | This is the old implementation of `evalBodyTxBody`. We keep it around to ensure that
-- the produced result hasn't changed
evaluateTransactionBalance ::
  (MaryEraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Value era
evaluateTransactionBalance pp dpstate utxo txBody =
  evaluateTransactionBalanceShelley pp dpstate utxo txBody <> (txBody ^. mintValueTxBodyF)

evaluateTransactionBalanceShelley ::
  (EraTxBody era, ShelleyEraTxCert era, EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Value era
evaluateTransactionBalanceShelley pp dpstate utxo txBody = consumed <-> produced
  where
    produced =
      sumUTxO (txouts txBody)
        <+> inject (txBody ^. feeTxBodyL <+> totalTxDeposits pp dpstate txBody)
    consumed =
      sumUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
        <> inject (refunds <> withdrawals)
    refunds = keyTxRefunds pp dpstate txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

-- | Randomly lookup pool params and staking credentials to add them as unregistration and
-- undelegation certificates respectively.
genTxBodyFrom ::
  (EraTxBody era, ShelleyEraTxCert era, Arbitrary (TxBody era), EraCertState era) =>
  CertState era ->
  UTxO era ->
  Gen (TxBody era)
genTxBodyFrom certState (UTxO u) = do
  txBody <- arbitrary
  inputs <- sublistOf (Map.keys u)
  unDelegCreds <-
    sublistOf (toList (UM.domain (UM.RewDepUView $ certState ^. certDStateL . dsUnifiedL)))
  deRegKeys <- sublistOf (Map.elems (certState ^. certPStateL . psStakePoolParamsL))
  certs <-
    shuffle $
      toList (txBody ^. certsTxBodyL)
        <> (UnRegTxCert <$> unDelegCreds)
        <> (RegPoolTxCert <$> deRegKeys)
  pure
    ( txBody
        & inputsTxBodyL .~ Set.fromList inputs
        & certsTxBodyL .~ SSeq.fromList certs
    )

propEvalBalanceTxBody ::
  (EraUTxO era, MaryEraTxBody era, ShelleyEraTxCert era, Arbitrary (TxBody era), EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceTxBody pp certState utxo =
  property $
    forAll (genTxBodyFrom certState utxo) $ \txBody ->
      evalBalanceTxBody pp lookupKeyDeposit (const Nothing) isRegPoolId utxo txBody
        `shouldBe` evaluateTransactionBalance pp certState utxo txBody
  where
    lookupKeyDeposit = lookupDepositDState (certState ^. certDStateL)
    isRegPoolId = (`Map.member` psStakePoolParams (certState ^. certPStateL))

propEvalBalanceShelleyTxBody ::
  (EraUTxO era, ShelleyEraTxCert era, Arbitrary (TxBody era), EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceShelleyTxBody pp certState utxo =
  property $
    forAll (genTxBodyFrom certState utxo) $ \txBody ->
      evalBalanceTxBody pp lookupKeyDeposit (const Nothing) isRegPoolId utxo txBody
        `shouldBe` evaluateTransactionBalanceShelley pp certState utxo txBody
  where
    lookupKeyDeposit = lookupDepositDState (certState ^. certDStateL)
    isRegPoolId = (`Map.member` psStakePoolParams (certState ^. certPStateL))

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
