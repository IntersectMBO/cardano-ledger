{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , verifyWitVKey
  , scriptsNeeded
  , txinsScript
  ) where

import           Lens.Micro ((^.))

import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

import           Coin (Coin (..))
import           Keys (KeyDiscriminator(..), DSIGNAlgorithm, HashAlgorithm, KeyPair, Signable, hash, hashKey, sKey, sign,
                     vKey, verify)
import           Ledger.Core (Relation (..))
import           PParams (PParams (..))
import           TxData (Addr (..), Credential (..), ScriptHash, StakeCredential, Tx (..),
                     TxBody (..), TxId (..), TxIn (..), TxOut (..), WitVKey (..), getRwdHK, inputs,
                     outputs, poolPubKey, txUpdate)
import           Updates (Update)

import           Delegation.Certificates (DCert (..), StakePools (..), cwitness, dvalue)

-- |The unspent transaction outputs.
newtype UTxO hashAlgo dsignAlgo
  = UTxO (Map (TxIn hashAlgo dsignAlgo) (TxOut hashAlgo dsignAlgo))
  deriving (Show, Eq, Ord)

instance Relation (UTxO hashAlgo dsignAlgo) where
  type Domain (UTxO hashAlgo dsignAlgo) = TxIn hashAlgo dsignAlgo
  type Range (UTxO hashAlgo dsignAlgo)  = TxOut hashAlgo dsignAlgo

  singleton k v = UTxO $ Map.singleton k v

  dom (UTxO utxo) = dom utxo

  range (UTxO utxo) = range utxo

  s ◁ (UTxO utxo) = UTxO $ s ◁ utxo

  s ⋪ (UTxO utxo) = UTxO $ s ⋪ utxo

  (UTxO utxo) ▷ s = UTxO $ utxo ▷ s

  (UTxO a) ∪ (UTxO b) = UTxO $ a ∪ b

  (UTxO a) ⨃ b = UTxO $ a ⨃ b

  vmax <=◁ (UTxO utxo) = UTxO $ vmax <=◁ utxo

  (UTxO utxo) ▷<= vmax = UTxO $ utxo ▷<= vmax

  (UTxO utxo) ▷>= vmin = UTxO $ utxo ▷>= vmin

  size (UTxO utxo) = size utxo

-- |Compute the id of a transaction.
txid
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TxBody hashAlgo dsignAlgo
  -> TxId hashAlgo dsignAlgo
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: TxBody hashAlgo dsignAlgo -> Set (TxIn hashAlgo dsignAlgo)
txins = flip (^.) inputs

-- |Compute the transaction outputs of a transaction.
txouts
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TxBody hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (tx ^. outputs) [0..]]
  where
    transId = txid tx

-- |Lookup a txin for a given UTxO collection
txinLookup
  :: TxIn hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
  -> Maybe (TxOut hashAlgo dsignAlgo)
txinLookup txin (UTxO utxo') = Map.lookup txin utxo'

-- |Verify a transaction body witness
verifyWitVKey
  :: ( DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> WitVKey hashAlgo dsignAlgo
  -> Bool
verifyWitVKey tx (WitVKey vkey sig) = verify vkey tx sig

-- |Create a witness for transaction
makeWitnessVKey
  :: ( DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> KeyPair 'Regular dsignAlgo
  -> WitVKey hashAlgo dsignAlgo
makeWitnessVKey tx keys = WitVKey (vKey keys) (sign (sKey keys) tx)

-- |Create witnesses for transaction
makeWitnessesVKey
  :: ( DSIGNAlgorithm dsignAlgo
     , HashAlgorithm hashAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> [KeyPair 'Regular dsignAlgo]
  -> Set (WitVKey hashAlgo dsignAlgo)
makeWitnessesVKey tx = Set.fromList . fmap (makeWitnessVKey tx)

-- |Determine the total balance contained in the UTxO.
balance :: UTxO hashAlgo dsignAlgo -> Coin
balance (UTxO utxo) = foldr addCoins 0 utxo
  where addCoins (TxOut _ a) b = a + b

-- |Determine the total deposit amount needed
deposits
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => PParams
  -> StakePools hashAlgo dsignAlgo
  -> [DCert hashAlgo dsignAlgo]
  -> Coin
deposits pc (StakePools stpools) cs = foldl f (Coin 0) cs'
  where
    f coin cert = coin + dvalue cert pc
    notRegisteredPool (RegPool pool) = Map.notMember (hashKey $ pool ^. poolPubKey) stpools
    notRegisteredPool _ = True
    cs' = filter notRegisteredPool cs

txup :: Tx hashAlgo dsignAlgo -> Update hashAlgo dsignAlgo
txup (Tx txbody _ _) = _txUpdate txbody

-- | Extract script hash from value address with script.
getScriptHash :: Addr hashAlgo dsignAlgo -> Maybe (ScriptHash hashAlgo dsignAlgo)
getScriptHash (AddrBase (ScriptHashObj hs) _)     = Just hs
getScriptHash (AddrEnterprise (ScriptHashObj hs)) = Just hs
getScriptHash _                                   = Nothing

scriptStakeCred
  :: StakeCredential hashAlgo dsignAlgo
  -> Maybe (ScriptHash hashAlgo dsignAlgo)
scriptStakeCred (KeyHashObj _ )    =  Nothing
scriptStakeCred (GenesisHashObj _ )    =  Nothing
scriptStakeCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => UTxO hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Set (ScriptHash hashAlgo dsignAlgo)
scriptsNeeded u tx =
  Set.fromList (Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
  `Set.union`
  Set.fromList (Maybe.mapMaybe (scriptStakeCred . getRwdHK) $ Map.keys withdrawals)
  `Set.union`
  Set.fromList (Maybe.mapMaybe (scriptStakeCred . cwitness) certificates)
  where unTxOut (TxOut a _) = a
        withdrawals = _wdrls $ _body tx
        UTxO u'' = txinsScript (txins $ _body tx) u <| u
        certificates = (toList . _certs . _body) tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript
  :: Set (TxIn hashAlgo dsignAlgo)
  -> UTxO hashAlgo dsignAlgo
  -> Set (TxIn hashAlgo dsignAlgo)
txinsScript txInps (UTxO u) =
  txInps `Set.intersection`
  Map.keysSet (Map.filter (\(TxOut a _) ->
                               case a of
                                 AddrBase (ScriptHashObj _) _     -> True
                                 AddrEnterprise (ScriptHashObj _) -> True
                                 _                                -> False) u)
