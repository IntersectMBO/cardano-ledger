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
    txCreatesNoScriptAddrs,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Prelude (Generic, NFData, NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (BaseRep (MapR), Embed (..), Exp (Base), HasExp (toExp), eval, rng)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Quiet
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    addrUsesScript,
    rewardAcntUsesScript,
  )
import Shelley.Spec.Ledger.BaseTypes (strictMaybeToMaybe)
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Core (Relation (..))
import Shelley.Spec.Ledger.Credential (Credential (..), credentialUsesScript)
import Shelley.Spec.Ledger.Crypto
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
import Shelley.Spec.Ledger.TxData
  ( DelegCert (..),
    MIRCert (..),
    PoolCert (..),
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

instance HasExp (UTxO crypto) (Map (TxIn crypto) (TxOut crypto)) where
  toExp (UTxO x) = Base MapR x

instance Embed (UTxO crypto) (Map (TxIn crypto) (TxOut crypto)) where
  toBase (UTxO x) = x
  fromBase x = (UTxO x)

-- | The unspent transaction outputs.
newtype UTxO crypto = UTxO {unUTxO :: Map (TxIn crypto) (TxOut crypto)}
  deriving (Eq, Ord, ToCBOR, FromCBOR, NoUnexpectedThunks, Generic, NFData)
  deriving (Show) via Quiet (UTxO crypto)

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

-- | Compute the id of a transaction.
txid ::
  Crypto crypto =>
  TxBody crypto ->
  TxId crypto
txid = TxId . hashAnnotated

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
  ( Typeable kr,
    Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  WitVKey crypto kr ->
  Bool
verifyWitVKey txbodyHash (WitVKey vkey sig) = verifySignedDSIGN vkey txbodyHash sig

-- | Create a witness for transaction
makeWitnessVKey ::
  forall crypto kr.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  KeyPair kr crypto ->
  WitVKey crypto 'Witness
makeWitnessVKey txbodyHash keys =
  WitVKey (asWitness $ vKey keys) (signedDSIGN @crypto (sKey keys) txbodyHash)

-- | Create witnesses for transaction
makeWitnessesVKey ::
  forall crypto kr.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  [KeyPair kr crypto] ->
  Set (WitVKey crypto ('Witness))
makeWitnessesVKey txbodyHash = Set.fromList . fmap (makeWitnessVKey txbodyHash)

-- | From a list of key pairs and a set of key hashes required for a multi-sig
-- scripts, return the set of required keys.
makeWitnessesFromScriptKeys ::
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Hash crypto (TxBody crypto) ->
  Map (KeyHash kr crypto) (KeyPair kr crypto) ->
  Set (KeyHash kr crypto) ->
  Set (WitVKey crypto ('Witness))
makeWitnessesFromScriptKeys txbodyHash hashKeyMap scriptHashes =
  let witKeys = Map.restrictKeys hashKeyMap scriptHashes
   in makeWitnessesVKey txbodyHash (Map.elems witKeys)

-- | Determine the total balance contained in the UTxO.
balance :: Crypto crypto => UTxO crypto -> Coin
balance (UTxO utxo) = foldr addCoins 0 utxo
  where
    addCoins (TxOut _ a) b = a + b

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
  Map (KeyHash 'StakePool crypto) (PoolParams crypto) ->
  [DCert crypto] ->
  Coin
totalDeposits pp stpools cs =
  (_keyDeposit pp) * numKeys + (_poolDeposit pp) * numNewPools
  where
    numKeys = intToCoin . length $ filter isRegKey cs
    pools = Set.fromList . Maybe.catMaybes $ fmap getKeyHashFromRegPool cs
    numNewPools = intToCoin . length $ pools `Set.difference` (Map.keysSet stpools)
    intToCoin = Coin . toInteger

getKeyHashFromRegPool :: DCert crypto -> Maybe (KeyHash 'StakePool crypto)
getKeyHashFromRegPool (DCertPool (RegPool p)) = Just . _poolPubKey $ p
getKeyHashFromRegPool _ = Nothing

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
  Crypto crypto =>
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

txCreatesNoScriptAddrs :: Crypto crypto => TxBody crypto -> Bool
txCreatesNoScriptAddrs txb =
  null outputsUsingScripts
    && null stakeAddrCertsUsingScripts
    && null poolRegCertsUsingScripts
    && null mirCertsUsingScripts
  where
    outputsUsingScripts =
      [out | out@(TxOut addr _) <- Set.toList (eval (rng (txouts txb))), addrUsesScript addr]
    stakeAddrCertsUsingScripts =
      [cert | cert@(DCertDeleg (RegKey sc)) <- toList (_certs txb), credentialUsesScript sc]
    poolRegCertsUsingScripts =
      [cert | cert@(DCertPool (RegPool pparams)) <- toList (_certs txb), rewardAcntUsesScript (_poolRAcnt pparams)]
    mirCertsUsingScripts =
      [cert | cert@(DCertMir mir) <- toList (_certs txb), any credentialUsesScript (Map.keys (mirRewards mir))]
