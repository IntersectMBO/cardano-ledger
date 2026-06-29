{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.UTxO (
  getConsumedMaryValue,
  getProducedMaryValue,
  burnedMultiAssets,
) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.State ()
import Cardano.Ledger.Mary.Value (MaryValue (..), filterMultiAsset, mapMaybeMultiAsset, policyID)
import Cardano.Ledger.Shelley.UTxO (
  ShelleyScriptsNeeded (..),
  getShelleyMinFeeTxUtxo,
  getShelleyScriptsNeeded,
  getShelleyWitsVKeyNeeded,
  shelleyConsumed,
  shelleyProducedValue,
 )
import Cardano.Ledger.State (
  EraUTxO (..),
  ScriptsProvided (..),
  UTxO,
  sumUTxO,
  txInsFilter,
 )
import Cardano.Ledger.Val (inject)
import Data.Foldable (fold)
import qualified Data.Set as Set
import Lens.Micro

instance EraUTxO MaryEra where
  type ScriptsNeeded MaryEra = ShelleyScriptsNeeded MaryEra

  consumed = shelleyConsumed

  getConsumedValue = getConsumedMaryValue

  getProducedValue pp isRegPoolId txBody =
    withTopTxLevelOnly txBody (getProducedMaryValue pp isRegPoolId)

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getMaryScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes

  getWitsVKeyNeeded = getShelleyWitsVKeyNeeded

  getMinFeeTxUtxo pp tx _ = getShelleyMinFeeTxUtxo pp tx

-- | Calculate the value consumed by the transation.
--
--   This differs from the corresponding Shelley function 'Shelley.coinConsumed'
--   since it works on Value and it also considers the "mint" field which
--   creates or destroys non-Ada tokens.
--
--   Note that this is slightly confusing, since it also covers non-Ada assets
--   _created_ by the transaction, depending on the sign of the quantities in
--   the mint field.
getConsumedMaryValue ::
  (MaryEraTxBody era, Value era ~ MaryValue) =>
  PParams era ->
  (Credential Staking -> Maybe Coin) ->
  (Credential DRepRole -> Maybe Coin) ->
  UTxO era ->
  TxBody l era ->
  MaryValue
getConsumedMaryValue pp lookupStakingDeposit lookupDRepDeposit utxo txBody =
  consumedValue <> MaryValue mempty mintedMultiAsset
  where
    mintedMultiAsset = filterMultiAsset (\_ _ -> (> 0)) $ txBody ^. mintTxBodyL
    {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx -}
    consumedValue =
      sumUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
        <> inject (refunds <> withdrawals)
    refunds = getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

getProducedMaryValue ::
  (MaryEraTxBody era, Value era ~ MaryValue) =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash StakePool -> Bool) ->
  TxBody TopTx era ->
  MaryValue
getProducedMaryValue pp isPoolRegistered txBody =
  shelleyProducedValue pp isPoolRegistered txBody <> burnedMultiAssets txBody

burnedMultiAssets :: MaryEraTxBody era => TxBody l era -> MaryValue
burnedMultiAssets txBody =
  MaryValue mempty $
    mapMaybeMultiAsset (\_ _ v -> if v < 0 then Just (negate v) else Nothing) $
      txBody ^. mintTxBodyL

-- | Computes the set of script hashes required to unlock the transaction inputs and the
-- withdrawals. Unlike the one from Shelley, this one also includes script hashes needed
-- for minting multi-assets in the transaction.
getMaryScriptsNeeded ::
  (ShelleyEraTxBody era, MaryEraTxBody era) =>
  UTxO era ->
  TxBody l era ->
  ShelleyScriptsNeeded era
getMaryScriptsNeeded u txBody =
  case getShelleyScriptsNeeded u txBody of
    ShelleyScriptsNeeded shelleyScriptsNeeded ->
      ShelleyScriptsNeeded $
        shelleyScriptsNeeded `Set.union` Set.map policyID (txBody ^. mintedTxBodyF)
