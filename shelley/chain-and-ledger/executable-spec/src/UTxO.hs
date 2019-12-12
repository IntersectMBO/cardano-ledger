{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- for the Relation instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : UTxO
Description : Simple UTxO Ledger

This module defines the types and functions for a simple UTxO Ledger
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}

module UTxO
  (
  -- * Primitives
    UTxO(..)
  -- * Functions
  , txid
  , txins
  , txinLookup
  , txouts
  , txUpdate
  , txup
  , balance
  , deposits
  , makeWitnessVKey
  , makeWitnessesVKey
  , makeGenWitnessVKey
  , makeGenWitnessesVKey
  , verifyWitVKey
  , scriptsNeeded
  , txinsScript
  ) where

import           Lens.Micro ((^.))

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

import           Cardano.Ledger.Shelley.Crypto
import           Coin (Coin (..))
import           Keys (KeyDiscriminator (..), KeyPair, Signable, hash, sKey, sign, vKey, verify)
import           Ledger.Core (Relation (..))
import           PParams (PParams (..))
import           TxData (Addr (..), Credential (..), ScriptHash, StakeCredential, Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxOut (..), WitVKey (..), getRwdCred,
                     inputs, outputs, poolPubKey, txUpdate)
import           Updates (Update)

import           Delegation.Certificates (DCert (..), StakePools (..), cwitness, dvalue,
                     requiresVKeyWitness)

-- |The unspent transaction outputs.
newtype UTxO crypto
  = UTxO (Map (TxIn crypto) (TxOut crypto))
  deriving (Show, Eq, Ord, NoUnexpectedThunks)

instance Relation (UTxO crypto) where
  type Domain (UTxO crypto) = TxIn crypto
  type Range (UTxO crypto)  = TxOut crypto

  singleton k v = UTxO $ Map.singleton k v

  dom (UTxO utxo) = dom utxo

  range (UTxO utxo) = range utxo

  s ◁ (UTxO utxo) = UTxO $ s ◁ utxo

  s ⋪ (UTxO utxo) = UTxO $ s ⋪ utxo

  (UTxO utxo) ▷ s = UTxO $ utxo ▷ s

  (UTxO utxo) ⋫ s = UTxO $ utxo ⋫ s

  (UTxO a) ∪ (UTxO b) = UTxO $ a ∪ b

  (UTxO a) ⨃ b = UTxO $ a ⨃ b

  vmax <=◁ (UTxO utxo) = UTxO $ vmax <=◁ utxo

  (UTxO utxo) ▷<= vmax = UTxO $ utxo ▷<= vmax

  (UTxO utxo) ▷>= vmin = UTxO $ utxo ▷>= vmin

  size (UTxO utxo) = size utxo

-- |Compute the id of a transaction.
txid
  :: Crypto crypto
  => TxBody crypto
  -> TxId crypto
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins
  :: TxBody crypto
  -> Set (TxIn crypto)
txins = flip (^.) inputs

-- |Compute the transaction outputs of a transaction.
txouts
  :: Crypto crypto
  => TxBody crypto
  -> UTxO crypto
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (tx ^. outputs) [0..]]
  where
    transId = txid tx

-- |Lookup a txin for a given UTxO collection
txinLookup
  :: TxIn crypto
  -> UTxO crypto
  -> Maybe (TxOut crypto)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- |Verify a transaction body witness
verifyWitVKey
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TxBody crypto
  -> WitVKey crypto
  -> Bool
verifyWitVKey tx (WitVKey vkey sig) = verify vkey tx sig
verifyWitVKey tx (WitGVKey vkey sig) = verify vkey tx sig

-- |Create a witness for transaction
makeWitnessVKey
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TxBody crypto
  -> KeyPair 'Regular crypto
  -> WitVKey crypto
makeWitnessVKey tx keys = WitVKey (vKey keys) (sign (sKey keys) tx)

-- |Create witnesses for transaction
makeWitnessesVKey
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TxBody crypto
  -> [KeyPair 'Regular crypto]
  -> Set (WitVKey crypto)
makeWitnessesVKey tx = Set.fromList . fmap (makeWitnessVKey tx)

-- |Create a genesis witness for transaction
makeGenWitnessVKey
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TxBody crypto
  -> KeyPair 'Genesis crypto
  -> WitVKey crypto
makeGenWitnessVKey tx keys = WitGVKey (vKey keys) (sign (sKey keys) tx)

-- |Create genesis witnesses for transaction
makeGenWitnessesVKey
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TxBody crypto
  -> [KeyPair 'Genesis crypto]
  -> Set (WitVKey crypto)
makeGenWitnessesVKey tx = Set.fromList . fmap (makeGenWitnessVKey tx)

-- |Determine the total balance contained in the UTxO.
balance :: UTxO crypto -> Coin
balance (UTxO utxo) = foldr addCoins 0 utxo
  where addCoins (TxOut _ a) b = a + b

-- |Determine the total deposit amount needed
deposits
  :: PParams
  -> StakePools crypto
  -> [DCert crypto]
  -> Coin
deposits pc (StakePools stpools) cs = foldl f (Coin 0) cs'
  where
    f coin cert = coin + dvalue cert pc
    notRegisteredPool (RegPool pool) = Map.notMember (pool ^. poolPubKey) stpools
    notRegisteredPool _ = True
    cs' = filter notRegisteredPool cs

txup :: Tx crypto -> Update crypto
txup (Tx txbody _ _) = _txUpdate txbody

-- | Extract script hash from value address with script.
getScriptHash :: Addr crypto -> Maybe (ScriptHash crypto)
getScriptHash (AddrBase (ScriptHashObj hs) _)     = Just hs
getScriptHash (AddrEnterprise (ScriptHashObj hs)) = Just hs
getScriptHash _                                   = Nothing

scriptStakeCred
  :: StakeCredential crypto
  -> Maybe (ScriptHash crypto)
scriptStakeCred (KeyHashObj _ )    =  Nothing
scriptStakeCred (GenesisHashObj _ )    =  Nothing
scriptStakeCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded
  :: UTxO crypto
  -> Tx crypto
  -> Set (ScriptHash crypto)
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
  `Set.union`
  Set.fromList (Maybe.mapMaybe (scriptStakeCred . getRwdCred) $ Map.keys withdrawals)
  `Set.union`
  Set.fromList (Maybe.mapMaybe (scriptStakeCred . cwitness) (filter requiresVKeyWitness certificates))
  where unTxOut (TxOut a _) = a
        withdrawals = _wdrls $ _body tx
        UTxO u'' = txinsScript (txins $ _body tx) u <| u
        certificates = (toList . _certs . _body) tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript
  :: Set (TxIn crypto)
  -> UTxO crypto
  -> Set (TxIn crypto)
txinsScript txInps (UTxO u) =
  txInps `Set.intersection`
  Map.keysSet (Map.filter (\(TxOut a _) ->
                               case a of
                                 AddrBase (ScriptHashObj _) _     -> True
                                 AddrEnterprise (ScriptHashObj _) -> True
                                 _                                -> False) u)
