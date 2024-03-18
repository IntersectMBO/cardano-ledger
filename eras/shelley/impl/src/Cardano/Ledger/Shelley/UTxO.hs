{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.UTxO (
  EraUTxO (..),
  ShelleyScriptsNeeded (..),
  scriptsNeeded,
  getShelleyScriptsNeeded,
  scriptCred,
  scriptStakeCred,
  getConsumedCoin,
  shelleyProducedValue,
  consumed,
  produced,
  txup,
  getShelleyMinFeeTxUtxo,
  getShelleyWitsVKeyNeeded,
  getShelleyWitsVKeyNeededNoGov,
  module UTxO,
)
where

import Cardano.Ledger.Address (Addr (..), bootstrapKeyHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import Cardano.Ledger.CertState (
  CertState (..),
  certDStateL,
  certPStateL,
  certVStateL,
  dsGenDelegs,
  lookupDepositDState,
  lookupDepositVState,
  psStakePoolParamsL,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), credKeyHashWitness, credScriptHash)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  GenDelegs (..),
  KeyHash (..),
  KeyRole (..),
  asWitness,
  genDelegKeyHash,
 )
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ProposedPPUpdates (ProposedPPUpdates), Update (..))
import Cardano.Ledger.Shelley.Tx ()
import Cardano.Ledger.Shelley.TxBody (
  ShelleyEraTxBody (..),
  Withdrawals (..),
  raCredential,
 )
import Cardano.Ledger.Shelley.TxCert (
  ShelleyEraTxCert,
  pattern DelegStakeTxCert,
  pattern RegTxCert,
  pattern UnRegTxCert,
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO as UTxO
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Control.SetAlgebra (eval, (◁))
import Data.Foldable (Foldable (fold), foldr', toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))

txup :: (EraTx era, ShelleyEraTxBody era) => Tx era -> Maybe (Update era)
txup tx = strictMaybeToMaybe (tx ^. bodyTxL . updateTxBodyL)
{-# DEPRECATED txup "In favor of `updateTxBodyL`" #-}

scriptStakeCred ::
  ShelleyEraTxCert era =>
  TxCert era ->
  Maybe (ScriptHash (EraCrypto era))
scriptStakeCred = \case
  RegTxCert _ -> Nothing
  UnRegTxCert cred -> credScriptHash cred
  DelegStakeTxCert cred _ -> credScriptHash cred
  _ -> Nothing
{-# DEPRECATED scriptStakeCred "In favor of `getScriptWitnessTxCert`" #-}

scriptCred :: Credential kr c -> Maybe (ScriptHash c)
scriptCred = credScriptHash
{-# DEPRECATED scriptCred "In favor of `credScriptHash`" #-}

-- | Computes the set of script hashes required to unlock the transaction inputs
-- and the withdrawals.
scriptsNeeded ::
  forall era.
  EraTx era =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash (EraCrypto era))
scriptsNeeded u tx =
  case getShelleyScriptsNeeded u (tx ^. bodyTxL) of
    ShelleyScriptsNeeded sn -> sn
{-# DEPRECATED scriptsNeeded "In favor of `getScriptsNeeded`" #-}

-- | Compute the subset of inputs of the set 'txIns' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScriptHashes ::
  EraTxOut era =>
  Set (TxIn (EraCrypto era)) ->
  UTxO era ->
  Set (ScriptHash (EraCrypto era))
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
consumed ::
  EraUTxO era =>
  PParams era ->
  CertState era ->
  UTxO era ->
  TxBody era ->
  Value era
consumed pp certState =
  getConsumedValue
    pp
    (lookupDepositDState $ certState ^. certDStateL)
    (lookupDepositVState $ certState ^. certVStateL)

-- | Compute the lovelace which are created by the transaction
-- For eras before Conway, VState is expected to have an empty Map for vsDReps, and so deposit summed up is zero.
produced ::
  EraUTxO era =>
  PParams era ->
  CertState era ->
  TxBody era ->
  Value era
produced pp certState =
  getProducedValue pp (flip Map.member $ certState ^. certPStateL . psStakePoolParamsL)

shelleyProducedValue ::
  EraTxBody era =>
  PParams era ->
  -- | Check whether a pool with a supplied PoolStakeId is already registered.
  (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
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
  (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
  UTxO era ->
  TxBody era ->
  Coin
getConsumedCoin pp lookupRefund utxo txBody =
  {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds dpstate tx -}
  coinBalance (txInsFilter utxo (txBody ^. inputsTxBodyL))
    <> refunds
    <> withdrawals
  where
    refunds = getTotalRefundsTxBody pp lookupRefund (const Nothing) txBody
    withdrawals = fold . unWithdrawals $ txBody ^. withdrawalsTxBodyL

newtype ShelleyScriptsNeeded era = ShelleyScriptsNeeded (Set (ScriptHash (EraCrypto era)))
  deriving (Eq, Show)

instance Crypto c => EraUTxO (ShelleyEra c) where
  type ScriptsNeeded (ShelleyEra c) = ShelleyScriptsNeeded (ShelleyEra c)

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
  GenDelegs (EraCrypto era) ->
  Set (KeyHash 'Witness (EraCrypto era))
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
  Set (KeyHash 'Witness (EraCrypto era))
getShelleyWitsVKeyNeededNoGov utxo' txBody =
  certAuthors
    `Set.union` inputAuthors
    `Set.union` owners
    `Set.union` wdrlAuthors
  where
    inputAuthors :: Set (KeyHash 'Witness (EraCrypto era))
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

    wdrlAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    wdrlAuthors = Map.foldrWithKey' accum Set.empty (unWithdrawals (txBody ^. withdrawalsTxBodyL))
      where
        accum key _ !ans =
          case credKeyHashWitness (raCredential key) of
            Nothing -> ans
            Just vkeyWit -> Set.insert vkeyWit ans
    owners :: Set (KeyHash 'Witness (EraCrypto era))
    owners = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum (RegPoolTxCert pool) !ans =
          Set.union
            (Set.map asWitness (ppOwners pool))
            ans
        accum _cert ans = ans
    certAuthors :: Set (KeyHash 'Witness (EraCrypto era))
    certAuthors = foldr' accum Set.empty (txBody ^. certsTxBodyL)
      where
        accum cert !ans =
          case getVKeyWitnessTxCert cert of
            Nothing -> ans
            Just vkeyWit -> Set.insert vkeyWit ans

getShelleyWitsVKeyNeeded ::
  forall era.
  (EraTx era, ShelleyEraTxBody era) =>
  CertState era ->
  UTxO era ->
  TxBody era ->
  Set (KeyHash 'Witness (EraCrypto era))
getShelleyWitsVKeyNeeded certState utxo txBody =
  getShelleyWitsVKeyNeededNoGov utxo txBody
    `Set.union` witsVKeyNeededGenDelegs txBody (dsGenDelegs (certState ^. certDStateL))
