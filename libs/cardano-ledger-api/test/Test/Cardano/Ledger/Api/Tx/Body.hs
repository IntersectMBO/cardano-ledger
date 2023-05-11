{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.Tx.Body (spec) where

import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.Tx.Body
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Compactible
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.TxCert (
  ShelleyDelegCert (..),
  isRegKey,
  pattern ShelleyTxCertDeleg,
 )
import Cardano.Ledger.Shelley.UTxO hiding (consumed, produced)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.Val
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Common

totalTxDeposits ::
  ShelleyEraTxBody era =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
totalTxDeposits pp dpstate txb =
  numKeys <×> pp ^. ppKeyDepositL <+> snd (foldl' accum (regpools, Coin 0) certs)
  where
    certs = toList (txb ^. certsTxBodyL)
    numKeys = length $ filter isRegKey certs
    regpools = psStakePoolParams (certPState dpstate)
    accum (!pools, !ans) (TxCertPool (RegPool poolparam)) =
      -- We don't pay a deposit on a pool that is already registered
      if Map.member (ppId poolparam) pools
        then (pools, ans)
        else (Map.insert (ppId poolparam) poolparam pools, ans <+> pp ^. ppPoolDepositL)
    accum ans _ = ans

keyTxRefunds ::
  ShelleyEraTxBody era =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Coin
keyTxRefunds pp dpstate tx = snd (foldl' accum (initialKeys, Coin 0) certs)
  where
    certs = tx ^. certsTxBodyL
    initialKeys = UM.RewardDeposits $ dsUnified $ certDState dpstate
    keyDeposit = UM.compactCoinOrError (pp ^. ppKeyDepositL)
    accum (!keys, !ans) (ShelleyTxCertDeleg (ShelleyRegCert k)) =
      -- Deposit is added locally to the growing 'keys'
      (UM.RewardDeposits $ UM.insert k (UM.RDPair mempty keyDeposit) keys, ans)
    accum (!keys, !ans) (ShelleyTxCertDeleg (ShelleyUnRegCert k)) =
      -- If the key is registered, lookup the deposit in the locally growing 'keys'
      -- if it is not registered, then just return ans
      case UM.lookup k keys of
        Just (UM.RDPair _ deposit) -> (keys, ans <+> fromCompact deposit)
        Nothing -> (keys, ans)
    accum ans _ = ans

-- | This is the old implementation of `evalBodyTxBody`. We keep it around to ensure that
-- the produced result hasn't changed
evaluateTransactionBalance ::
  (EraUTxO era, MaryEraTxBody era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Value era
evaluateTransactionBalance pp dpstate (UTxO u) txBody = consumed <-> produced
  where
    produced =
      balance (txouts txBody)
        <+> inject (txBody ^. feeTxBodyL <+> totalTxDeposits pp dpstate txBody)
    consumed =
      (txBody ^. mintValueTxBodyF)
        <> balance (UTxO (Map.restrictKeys u (txBody ^. inputsTxBodyL)))
        <> inject (refunds <> withdrawals)
    refunds = keyTxRefunds pp dpstate txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

-- | Randomly lookup pool params and staking credentials to add them as unregistration and
-- undelegation certificates respectively.
genTxBodyFrom ::
  (ShelleyEraTxBody era, Arbitrary (TxBody era)) =>
  CertState era ->
  UTxO era ->
  Gen (TxBody era)
genTxBodyFrom CertState {certDState, certPState} (UTxO u) = do
  txBody <- arbitrary
  inputs <- sublistOf (Map.keys u)
  unDelegCreds <- sublistOf (toList (UM.domain (UM.RewardDeposits $ dsUnified certDState)))
  deRegKeys <- sublistOf (Map.elems (psStakePoolParams certPState))
  certs <-
    shuffle $
      toList (txBody ^. certsTxBodyL)
        <> (ShelleyTxCertDeleg . ShelleyUnRegCert <$> unDelegCreds)
        <> (TxCertPool . RegPool <$> deRegKeys)
  pure
    ( txBody
        & inputsTxBodyL .~ Set.fromList inputs
        & certsTxBodyL .~ SSeq.fromList certs
    )

propEvalBalanceTxBody ::
  (EraUTxO era, MaryEraTxBody era, Arbitrary (TxBody era)) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  Property
propEvalBalanceTxBody pp dpState utxo =
  property $
    forAll (genTxBodyFrom dpState utxo) $ \txBody ->
      evalBalanceTxBody pp lookupRefund isRegPoolId utxo txBody
        `shouldBe` evaluateTransactionBalance pp dpState utxo txBody
  where
    lookupRefund = lookupDepositDState (certDState dpState)
    isRegPoolId = (`Map.member` psStakePoolParams (certPState dpState))

spec :: Spec
spec =
  describe "TxBody" $ do
    describe "MaryEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @Mary
    describe "AlonzoEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @Alonzo
    describe "BabbageEra" $ do
      prop "evalBalanceTxBody" $ propEvalBalanceTxBody @Babbage

-- TODO: bring back when TxCerts become a type family
-- describe "ConwayEra" $ do
--   prop "evalBalanceTxBody" $ propEvalBalanceTxBody @Conway
