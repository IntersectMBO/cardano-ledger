{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- for the Relation instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : UTxO
-- Description : Simple UTxO Ledger
--
-- This module defines the types and functions for a simple UTxO Ledger
-- as specified in /A Simplified Formal Specification of a UTxO Ledger/.
module Shelley.Spec.Ledger.UTxO
  ( -- * Primitives
    UTxO (..),

    -- * Functions
    hashTxBody,
    txid,
    txins,
    txinLookup,
    txouts,
    txup,
    balance,
    totalDeposits,
    makeWitnessVKey,
    makeWitnessesVKey,
    makeWitnessesFromScriptKeys,
    verifyWitVKey,
    scriptsNeeded,
    txinsScript,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.Hash (hashWithSerialiser)
import Cardano.Prelude (Generic, NFData, NoUnexpectedThunks (..))
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Core (Relation (..))
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    StakePools (..),
    dvalue,
    requiresVKeyWitness,
  )
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    Hash,
    IsKeyRole,
    KeyHash (..),
    KeyPair (..),
    WitnessFor,
    asWitness,
    signedDSIGN,
    verifySignedDSIGN,
  )
import Shelley.Spec.Ledger.PParams (PParams, Update)
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxData
  ( PoolCert (..),
    PoolParams (..),
    TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
    WitVKey (..),
    getRwdCred,
    pattern DeRegKey,
    pattern Delegate,
    pattern Delegation,
  )

-- | The unspent transaction outputs.
newtype UTxO crypto
  = UTxO (Map (TxIn crypto) (TxOut crypto))
  deriving (Show, Eq, Ord, ToCBOR, FromCBOR, NoUnexpectedThunks, Generic, NFData)

instance Relation (UTxO crypto) where
  type Domain (UTxO crypto) = TxIn crypto
  type Range (UTxO crypto) = TxOut crypto

  singleton k v = UTxO $ Map.singleton k v

  dom (UTxO utxo) = dom utxo

  range (UTxO utxo) = range utxo

  s ◁ (UTxO utxo) = UTxO $ s ◁ utxo

  s ⋪ (UTxO utxo) = UTxO $ s ⋪ utxo

  (UTxO utxo) ▷ s = UTxO $ utxo ▷ s

  (UTxO utxo) ⋫ s = UTxO $ utxo ⋫ s

  (UTxO a) ∪ (UTxO b) = UTxO $ a ∪ b

  (UTxO a) ⨃ (UTxO b) = UTxO $ a ⨃ b

  size (UTxO utxo) = size utxo

  {-# INLINE haskey #-}
  haskey k (UTxO x) = case Map.lookup k x of Just _ -> True; Nothing -> False

  {-# INLINE addpair #-}
  addpair k v (UTxO x) = UTxO (Map.insertWith (\y _z -> y) k v x)

  {-# INLINE removekey #-}
  removekey k (UTxO m) = UTxO (Map.delete k m)

-- | Compute the hash of a transaction body.
hashTxBody ::
  Crypto crypto =>
  TxBody crypto ->
  Hash crypto (TxBody crypto)
hashTxBody = hashWithSerialiser toCBOR

-- | Compute the id of a transaction.
txid ::
  Crypto crypto =>
  TxBody crypto ->
  TxId crypto
txid = TxId . hashTxBody

-- | Compute the UTxO inputs of a transaction.
txins ::
  Crypto crypto =>
  TxBody crypto ->
  Set (TxIn crypto)
txins = _inputs

-- | Compute the transaction outputs of a transaction.
txouts ::
  Crypto crypto =>
  TxBody crypto ->
  UTxO crypto
txouts tx =
  UTxO $
    Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (toList $ _outputs tx) [0 ..]]
  where
    transId = txid tx

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn crypto ->
  UTxO crypto ->
  Maybe (TxOut crypto)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- | Verify a transaction body witness
verifyWitVKey ::
  ( IsKeyRole kr crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  WitVKey crypto kr ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash sig

-- | Create a witness for transaction
makeWitnessVKey ::
  forall crypto kr.
  ( IsKeyRole kr crypto,
    IsKeyRole (WitnessFor kr) crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  KeyPair kr crypto ->
  WitVKey crypto (WitnessFor kr)
makeWitnessVKey txbodyHash keys =
  WitVKey (asWitness $ vKey keys) (signedDSIGN @crypto (sKey keys) txbodyHash)

-- | Create witnesses for transaction
makeWitnessesVKey ::
  forall crypto kr.
  ( IsKeyRole kr crypto,
    IsKeyRole (WitnessFor kr) crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  [KeyPair kr crypto] ->
  Set (WitVKey crypto (WitnessFor kr))
makeWitnessesVKey txbodyHash = Set.fromList . fmap (makeWitnessVKey txbodyHash)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  ( IsKeyRole kr crypto,
    IsKeyRole (WitnessFor kr) crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  Map (KeyHash kr crypto) (KeyPair kr crypto) ->
  Set (KeyHash kr crypto) ->
  Set (WitVKey crypto (WitnessFor kr))
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in makeWitnessesVKey txbodyHash (Map.elems witKeys)

-- | Determine the total balance contained in the UTxO.
balance :: UTxO crypto -> Coin
balance (UTxO utxo) = foldr addCoins 0 utxo
  where
    addCoins (TxOut _ a) b = a + b

-- | Determine the total deposit amount needed
totalDeposits ::
  PParams ->
  StakePools crypto ->
  [DCert crypto] ->
  Coin
totalDeposits pc (StakePools stpools) cs = foldl' f (Coin 0) cs'
  where
    f coin cert = coin + dvalue cert pc
    notRegisteredPool (DCertPool (RegPool pool)) =
      Map.notMember (_poolPubKey pool) stpools
    notRegisteredPool _ = True
    cs' = filter notRegisteredPool cs

txup :: Crypto crypto => Tx crypto -> Maybe (Update crypto)
txup (Tx txbody _ _) = strictMaybeToMaybe (_txUpdate txbody)

-- | Extract script hash from value address with script.
getScriptHash :: Addr crypto -> Maybe (ScriptHash crypto)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

scriptStakeCred ::
  DCert crypto ->
  Maybe (ScriptHash crypto)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred ::
  Credential kr crypto ->
  Maybe (ScriptHash crypto)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded ::
  Crypto crypto =>
  UTxO crypto ->
  Tx crypto ->
  Set (ScriptHash crypto)
scriptsNeeded (u@(UTxO v)) tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
    `Set.union` Set.fromList (Maybe.mapMaybe (scriptCred . getRwdCred) $ Map.keys withdrawals)
    `Set.union` Set.fromList (Maybe.mapMaybe scriptStakeCred (filter requiresVKeyWitness certificates))
  where
    unTxOut (TxOut a _) = a
    withdrawals = unWdrl $ _wdrls $ _body tx
    u'' = Map.restrictKeys v (txinsScript (txins $ _body tx) u)
    certificates = (toList . _certs . _body) tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript ::
  Set (TxIn crypto) ->
  UTxO crypto ->
  Set (TxIn crypto)
txinsScript txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps that are locked in u
    add input ans = case Map.lookup input u of
      Just (TxOut (Addr _ (ScriptHashObj _) _) _) -> Set.insert input ans
      Just _ -> ans
      Nothing -> ans
