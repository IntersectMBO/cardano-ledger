{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- for the Relation instance
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Cardano.Ledger.Era
import Cardano.Prelude (Generic, NFData, NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp))
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Relation (Relation (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Quiet
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import Shelley.Spec.Ledger.Coin (Coin (..), word64ToCoin)
import Shelley.Spec.Ledger.Credential (Credential (..))
import Shelley.Spec.Ledger.Delegation.Certificates
  ( DCert (..),
    isRegKey,
    requiresVKeyWitness,
  )
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    Hash,
    KeyHash (..),
    KeyPair (..),
    KeyRole (StakePool, Witness),
    asWitness,
    signedDSIGN,
    verifySignedDSIGN,
  )
import Shelley.Spec.Ledger.PParams (PParams, Update, _keyDeposit, _poolDeposit)
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxBody
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
import qualified Cardano.Ledger.Val as Val

instance HasExp (UTxO era) (Map (TxIn era) (TxOut era)) where
  toExp (UTxO x) = Base MapR x

instance Embed (UTxO era) (Map (TxIn era) (TxOut era)) where
  toBase (UTxO x) = x
  fromBase x = (UTxO x)

-- | The unspent transaction outputs.
newtype UTxO era = UTxO {unUTxO :: Map (TxIn era) (TxOut era)}
  deriving (Eq, Ord, ToCBOR, FromCBOR, NoUnexpectedThunks, Generic, NFData)
  deriving (Show) via Quiet (UTxO era)

instance Relation (UTxO era) where
  type Domain (UTxO era) = TxIn era
  type Range (UTxO era) = TxOut era

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

-- | Compute the id of a transaction.
txid ::
  Era era =>
  TxBody era ->
  TxId era
txid = TxId . hashAnnotated

-- | Compute the UTxO inputs of a transaction.
txins ::
  Era era =>
  TxBody era ->
  Set (TxIn era)
txins = _inputs

-- | Compute the transaction outputs of a transaction.
txouts ::
  Era era =>
  TxBody era ->
  UTxO era
txouts tx =
  UTxO $
    Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (toList $ _outputs tx) [0 ..]]
  where
    transId = txid tx

-- | Lookup a txin for a given UTxO collection
txinLookup ::
  TxIn era ->
  UTxO era ->
  Maybe (TxOut era)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- | Verify a transaction body witness
verifyWitVKey ::
  ( Typeable kr,
    Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Hash era (TxBody era) ->
  WitVKey era kr ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash sig

-- | Create a witness for transaction
makeWitnessVKey ::
  forall era kr.
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Hash era (TxBody era) ->
  KeyPair kr era ->
  WitVKey era 'Witness
makeWitnessVKey txbodyHash keys =
  WitVKey (asWitness $ vKey keys) (signedDSIGN @era (sKey keys) txbodyHash)

-- | Create witnesses for transaction
makeWitnessesVKey ::
  forall era kr.
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Hash era (TxBody era) ->
  [KeyPair kr era] ->
  Set (WitVKey era ('Witness))
makeWitnessesVKey txbodyHash = Set.fromList . fmap (makeWitnessVKey txbodyHash)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Hash era (TxBody era) ->
  Map (KeyHash kr era) (KeyPair kr era) ->
  Set (KeyHash kr era) ->
  Set (WitVKey era ('Witness))
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in makeWitnessesVKey txbodyHash (Map.elems witKeys)

-- | Determine the total balance contained in the UTxO.
balance :: UTxO era -> Coin
balance (UTxO utxo) = Map.foldl' addCoins mempty utxo
  where
    addCoins !b (TxOutCompact _ (word64ToCoin -> a)) = a <> b

-- | Determine the total deposit amount needed.
-- The block may (legitimately) contain multiple registration certificates
-- for the same pool, where the first will be treated as a registration and
-- any subsequent ones as re-registration. As such, we must only take a
-- deposit for the first such registration.
--
-- Note that this is not an issue for key registrations since subsequent
-- registration certificates would be invalid.
totalDeposits ::
  PParams ->
  Map (KeyHash 'StakePool era) (PoolParams era) ->
  [DCert era] ->
  Coin
totalDeposits pp stpools cs =
  Val.scale numKeys (_keyDeposit pp) <> Val.scale numNewPools (_poolDeposit pp)
  where
    numKeys = length $ filter isRegKey cs
    pools = Set.fromList . Maybe.catMaybes $ fmap getKeyHashFromRegPool cs
    numNewPools = length $ pools `Set.difference` (Map.keysSet stpools)

getKeyHashFromRegPool :: DCert era -> Maybe (KeyHash 'StakePool era)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just . _poolPubKey $ p
getKeyHashFromRegPool _ = Nothing

txup :: Era era => Tx era -> Maybe (Update era)
txup (Tx txbody _ _) = strictMaybeToMaybe (_txUpdate txbody)

-- | Extract script hash from value address with script.
getScriptHash :: Addr era -> Maybe (ScriptHash era)
getScriptHash (Addr _ (ScriptHashObj hs) _) = Just hs
getScriptHash _ = Nothing

scriptStakeCred ::
  DCert era ->
  Maybe (ScriptHash era)
scriptStakeCred (DCertDeleg (DeRegKey (KeyHashObj _))) = Nothing
scriptStakeCred (DCertDeleg (DeRegKey (ScriptHashObj hs))) = Just hs
scriptStakeCred (DCertDeleg (Delegate (Delegation (KeyHashObj _) _))) = Nothing
scriptStakeCred (DCertDeleg (Delegate (Delegation (ScriptHashObj hs) _))) = Just hs
scriptStakeCred _ = Nothing

scriptCred ::
  Credential kr era ->
  Maybe (ScriptHash era)
scriptCred (KeyHashObj _) = Nothing
scriptCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded ::
  Era era =>
  UTxO era ->
  Tx era ->
  Set (ScriptHash era)
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
    `Set.union` Set.fromList (Maybe.mapMaybe (scriptCred . getRwdCred) $ Map.keys withdrawals)
    `Set.union` Set.fromList (Maybe.mapMaybe scriptStakeCred (filter requiresVKeyWitness certificates))
  where
    unTxOut (TxOut a _) = a
    withdrawals = unWdrl $ _wdrls $ _body tx
    UTxO u'' = (txinsScript (txins $ _body tx) u) ◁ u
    -- u'' = Map.restrictKeys v (txinsScript (txins $ _body tx) u)  TODO
    certificates = (toList . _certs . _body) tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript ::
  Era era =>
  Set (TxIn era) ->
  UTxO era ->
  Set (TxIn era)
txinsScript txInps (UTxO u) = foldr add Set.empty txInps
  where
    -- to get subset, start with empty, and only insert those inputs in txInps that are locked in u
    add input ans = case Map.lookup input u of
      Just (TxOut (Addr _ (ScriptHashObj _) _) _) -> Set.insert input ans
      Just _ -> ans
      Nothing -> ans
