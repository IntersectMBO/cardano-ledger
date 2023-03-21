{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.UTxO (
  ShelleyScriptsNeeded (..),
  scriptsNeeded,
  getShelleyScriptsNeeded,
  scriptCred,
  scriptStakeCred,
  getConsumedCoin,
  getProducedValue,
  consumed,
  produced,
  txup,
  module UTxO,
)
where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (strictMaybeToMaybe)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DPState (DPState (..), PState (..), lookupDepositDState)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Shelley.Delegation.Certificates (
  DCert (..),
  requiresVKeyWitness,
 )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState.RefundsAndDeposits (
  keyCertsRefunds,
  totalCertsDeposits,
 )
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  getRwdCred,
  pattern DeRegKey,
  pattern Delegate,
  pattern Delegation,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO as UTxO
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Data.Foldable (Foldable (fold), toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((^.))

txup :: (EraTx era, ShelleyEraTxBody era, ProtVerAtMost era 8) => Tx era -> Maybe (Update era)
txup tx = strictMaybeToMaybe (tx ^. bodyTxL . updateTxBodyL)

scriptStakeCred :: DCert c -> Maybe (ScriptHash c)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred :: Credential kr c -> Maybe (ScriptHash c)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transaction inputs
-- and the withdrawals.
scriptsNeeded ::
  forall era.
  (EraTx era, ShelleyEraTxBody era) =>
  UTxO era ->
  Tx era ->
  Set.Set (ScriptHash (EraCrypto era))
scriptsNeeded u tx =
  case getShelleyScriptsNeeded u (tx ^. bodyTxL) of
    ShelleyScriptsNeeded sn -> sn
{-# DEPRECATED scriptsNeeded "In favor of `getScriptsNeeded`" #-}

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  EraTxOut era =>
  Set.Set (TxIn (EraCrypto era)) ->
  UTxO era ->
  Set.Set (ScriptHash (EraCrypto era))
txinsScriptHashes txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps
    -- that are locked in u
    add input ans = case Map.lookup input u of
      Just txOut -> case txOut ^. addrTxOutL of
        Addr _ (ScriptHashObj h) _ -> Set.insert h ans
        _ -> ans
      Nothing -> ans

getShelleyScriptsNeeded ::
  ShelleyEraTxBody era =>
  UTxO era ->
  TxBody era ->
  ShelleyScriptsNeeded era
getShelleyScriptsNeeded u txBody =
  ShelleyScriptsNeeded
    ( scriptHashes
        `Set.union` Set.fromList
          [sh | w <- withdrawals, Just sh <- [scriptCred (getRwdCred w)]]
        `Set.union` Set.fromList
          [sh | c <- certificates, requiresVKeyWitness c, Just sh <- [scriptStakeCred c]]
    )
  where
    withdrawals = Map.keys (unWithdrawals (txBody ^. withdrawalsTxBodyL))
    scriptHashes = txinsScriptHashes (txBody ^. inputsTxBodyL) u
    certificates = toList (txBody ^. certsTxBodyG)

consumed ::
  EraUTxO era =>
  PParams era ->
  DPState (EraCrypto era) ->
  UTxO era ->
  TxBody era ->
  Value era
consumed pp dpstate = getConsumedValue pp (lookupDepositDState (dpsDState dpstate))

-- | Compute the lovelace which are created by the transaction
produced ::
  ShelleyEraTxBody era =>
  PParams era ->
  DPState (EraCrypto era) ->
  TxBody era ->
  Value era
produced pp dpstate =
  getProducedValue pp (`Map.member` psStakePoolParams (dpsPState dpstate))

getProducedValue ::
  ShelleyEraTxBody era =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
  TxBody era ->
  Value era
getProducedValue pp isRegPoolId txBody =
  sumAllValue (txBody ^. outputsTxBodyL)
    <+> Val.inject
      (txBody ^. feeTxBodyL <+> totalCertsDeposits pp isRegPoolId (txBody ^. certsTxBodyG))

-- | Compute the lovelace which are destroyed by the transaction
getConsumedCoin ::
  ShelleyEraTxBody era =>
  PParams era ->
  (StakeCredential (EraCrypto era) -> Maybe Coin) ->
  UTxO era ->
  TxBody era ->
  Coin
getConsumedCoin pp lookupRefund (UTxO u) txBody =
  {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds dpstate tx -}
  coinBalance (UTxO (Map.restrictKeys u (txBody ^. inputsTxBodyL)))
    <> refunds
    <> withdrawals
  where
    refunds = keyCertsRefunds pp lookupRefund (txBody ^. certsTxBodyG)
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

newtype ShelleyScriptsNeeded era = ShelleyScriptsNeeded (Set.Set (ScriptHash (EraCrypto era)))
  deriving (Eq, Show)

instance Crypto c => EraUTxO (ShelleyEra c) where
  type ScriptsNeeded (ShelleyEra c) = ShelleyScriptsNeeded (ShelleyEra c)

  getConsumedValue = getConsumedCoin

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptsHashes) = scriptsHashes
