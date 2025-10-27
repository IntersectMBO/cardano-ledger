{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.UTxO (
  EraUTxO (..),
  ShelleyScriptsNeeded (..),
  getShelleyScriptsNeeded,
  getConsumedCoin,
  shelleyProducedValue,
  shelleyConsumed,
  produced,
  getShelleyMinFeeTxUtxo,
  getShelleyWitsVKeyNeeded,
  getShelleyWitsVKeyNeededNoGov,
  module UTxO,
) where

import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Keys (
  GenDelegs (..),
  asWitness,
  genDelegKeyHash,
 )
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (ProposedPPUpdates), Update (..))
import Cardano.Ledger.Shelley.State ()
import Cardano.Ledger.Shelley.Tx ()
import Cardano.Ledger.Shelley.TxBody (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  raCredential,
 )
import Cardano.Ledger.State (
  EraCertState (..),
  StakePoolParams (..),
  dsGenDelegs,
  lookupDepositDState,
  psStakePoolsL,
 )
import Cardano.Ledger.State as UTxO (
  CanGetUTxO (..),
  CanSetUTxO (..),
  EraUTxO (..),
  ScriptsProvided (..),
  UTxO (..),
  areAllAdaOnly,
  getScriptHash,
  sumAllCoin,
  sumAllValue,
  sumCoinUTxO,
  sumUTxO,
  txInsFilter,
  txinLookup,
  txins,
  txouts,
  verifyWitVKey,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (Foldable (fold), foldr', toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Lens.Micro ((^.))

-- | Compute the subset of inputs of the set 'txIns' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  EraTxOut era =>
  Set TxIn ->
  UTxO era ->
  Set ScriptHash
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
  EraTxBody era =>
  UTxO era ->
  TxBody era ->
  ShelleyScriptsNeeded era
getShelleyScriptsNeeded u txBody =
  ShelleyScriptsNeeded
    ( scriptHashes
        `Set.union` Set.fromList
          [sh | w <- withdrawals, Just sh <- [credScriptHash (raCredential w)]]
        `Set.union` Set.fromList
          [sh | c <- certificates, Just sh <- [getScriptWitnessTxCert c]]
    )
  where
    withdrawals = Map.keys (unWithdrawals (txBody ^. withdrawalsTxBodyL))
    scriptHashes = txinsScriptHashes (txBody ^. inputsTxBodyL) u
    certificates = toList (txBody ^. certsTxBodyL)

-- | For eras before Conway, VState is expected to have an empty Map for vsDReps, and so deposit summed up is zero.
shelleyConsumed ::
  (EraUTxO era, EraCertState era) =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Value era
shelleyConsumed pp certState =
  getConsumedValue
    pp
    (lookupDepositDState $ certState ^. certDStateL)
    (const Nothing)

-- | Compute the lovelace which are created by the transaction
-- For eras before Conway, VState is expected to have an empty Map for vsDReps, and so deposit summed up is zero.
produced ::
  (EraUTxO era, EraCertState era) =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Value era
produced pp certState =
  getProducedValue pp (flip Map.member $ certState ^. certPStateL . psStakePoolsL)

shelleyProducedValue ::
  EraTxBody era =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool -> Bool) ->
  TxBody era ->
  Value era
shelleyProducedValue pp isRegPoolId txBody =
  sumAllValue (txBody ^. outputsTxBodyL)
    <+> Val.inject
      (txBody ^. feeTxBodyL <+> getTotalDepositsTxBody pp isRegPoolId txBody)

-- | Compute the lovelace which are destroyed by the transaction. This implementation is
-- suitable for Shelley and Allegra only.
getConsumedCoin ::
  EraTxBody era =>
  PParams era ->
  (Credential 'Staking -> Maybe Coin) ->
  UTxO era ->
  TxBody era ->
  Coin
getConsumedCoin pp lookupRefund utxo txBody =
  {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds dpstate tx -}
  sumCoinUTxO (txInsFilter utxo (txBody ^. inputsTxBodyL))
    <> refunds
    <> withdrawals
  where
    refunds = getTotalRefundsTxBody pp lookupRefund (const Nothing) txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

newtype ShelleyScriptsNeeded era = ShelleyScriptsNeeded (Set ScriptHash)
  deriving (Eq, Show, Generic)

instance EraUTxO ShelleyEra where
  type ScriptsNeeded ShelleyEra = ShelleyScriptsNeeded ShelleyEra

  consumed = shelleyConsumed

  getConsumedValue pp lookupKeyDeposit _ = getConsumedCoin pp lookupKeyDeposit

  getProducedValue = shelleyProducedValue

  getScriptsProvided _ tx = ScriptsProvided (tx ^. witsTxL . scriptTxWitsL)

  getScriptsNeeded = getShelleyScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptsHashes) = scriptsHashes

  getWitsVKeyNeeded = getShelleyWitsVKeyNeeded

  getMinFeeTxUtxo pp tx _ = getShelleyMinFeeTxUtxo pp tx

-- We don't consider the reference scripts in the calculation before Conway
getShelleyMinFeeTxUtxo :: EraTx era => PParams era -> Tx era -> Coin
getShelleyMinFeeTxUtxo pparams tx = getMinFeeTx pparams tx 0

-- | Collect the set of hashes of keys that needs to sign a
--  given transaction. This set consists of the txin owners,
--  certificate authors, and withdrawal reward accounts.
witsVKeyNeededGenDelegs ::
  forall era.
  ShelleyEraTxBody era =>
  TxBody era ->
  GenDelegs ->
  Set (KeyHash 'Witness)
witsVKeyNeededGenDelegs txBody (GenDelegs genDelegs) =
  asWitness `Set.map` proposedUpdatesWitnesses (txBody ^. updateTxBodyL)
  where
    -- Calculate the set of hash keys of the required witnesses for update
    -- proposals.
    proposedUpdatesWitnesses = \case
      SNothing -> Set.empty
      SJust (Update (ProposedPPUpdates pup) _) ->
        Set.map asWitness . Set.fromList $ Map.elems updateKeys''
        where
          updateKeys' = eval (Map.keysSet pup ◁ genDelegs)
          updateKeys'' = Map.map genDelegKeyHash updateKeys'

-- | Extract witnesses from UTxO and TxBody. Does not enforce witnesses for governance
-- related Keys, i.e. `GenDelegs`
getShelleyWitsVKeyNeededNoGov ::
  forall era.
  EraTx era =>
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness)
getShelleyWitsVKeyNeededNoGov utxo' txBody =
  certAuthors
    `Set.union` inputAuthors
    `Set.union` owners
    `Set.union` wdrlAuthors
  where
    inputAuthors :: Set (KeyHash 'Witness)
    inputAuthors = foldr' accum Set.empty (txBody ^. spendableInputsTxBodyF)
      where
        accum txin !ans =
          case txinLookup txin utxo' of
            Just txOut ->
              case txOut ^. addrTxOutL of
                Addr _ (KeyHashObj pay) _ -> Set.insert (asWitness pay) ans
                AddrBootstrap bootAddr ->
                  Set.insert (asWitness (bootstrapKeyHash bootAddr)) ans
                _ -> ans
            Nothing -> ans

    wdrlAuthors :: Set (KeyHash 'Witness)
    wdrlAuthors = Map.foldrWithKey' accum Set.empty (unWithdrawals (txBody ^. withdrawalsTxBodyL))
      where
        accum key _ !ans =
          case credKeyHashWitness (raCredential key) of
            Nothing -> ans
            Just vkeyWit -> Set.insert vkeyWit ans
    owners :: Set (KeyHash 'Witness)
    owners = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum (RegPoolTxCert pool) !ans =
          Set.union
            (Set.map asWitness (sppOwners pool))
            ans
        accum _cert ans = ans
    certAuthors :: Set (KeyHash 'Witness)
    certAuthors = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum cert !ans =
          case getVKeyWitnessTxCert cert of
            Nothing -> ans
            Just vkeyWit -> Set.insert vkeyWit ans

getShelleyWitsVKeyNeeded ::
  forall era.
  (EraTx era, ShelleyEraTxBody era, EraCertState era) =>
  CertState era ->
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness)
getShelleyWitsVKeyNeeded certState utxo txBody =
  getShelleyWitsVKeyNeededNoGov utxo txBody
    `Set.union` witsVKeyNeededGenDelegs txBody (dsGenDelegs (certState ^. certDStateL))
