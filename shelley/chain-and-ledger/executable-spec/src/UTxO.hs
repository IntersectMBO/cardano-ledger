{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , (<|)
  , (</|)
  , dom
  , union
  , makeWitnessVKey
  , makeWitnessesVKey
  , verifyWitVKey
  , scriptsNeeded
  , txinsScript
  ) where

import           Lens.Micro ((^.))

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import           Data.Set (Set)
import qualified Data.Set as Set

import           Coin (Coin (..))
import           Keys
import           PParams (PParams (..))
import           Tx
import           TxData
import           Updates (Update)

import           Delegation.Certificates (DCert (..), StakePools (..), cwitness, dvalue)

-- |The unspent transaction outputs.
newtype UTxO hashAlgo dsignAlgo
  = UTxO (Map (TxIn hashAlgo dsignAlgo) (TxOut hashAlgo dsignAlgo))
  deriving (Show, Eq, Ord)

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
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> WitVKey hashAlgo dsignAlgo
  -> Bool
verifyWitVKey tx (WitVKey vkey sig) = verify vkey tx sig

-- |Create a witness for transaction
makeWitnessVKey
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> KeyPair dsignAlgo
  -> WitVKey hashAlgo dsignAlgo
makeWitnessVKey tx keys = WitVKey (vKey keys) (sign (sKey keys) tx)

-- |Create witnesses for transaction
makeWitnessesVKey
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TxBody hashAlgo dsignAlgo
  -> [KeyPair dsignAlgo]
  -> Set (WitVKey hashAlgo dsignAlgo)
makeWitnessesVKey tx = Set.fromList . fmap (makeWitnessVKey tx)

-- |Domain restriction
(<|)
  :: Set (TxIn hashAlgo dsignAlgo)
  -> UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion
(</|)
  :: Set (TxIn hashAlgo dsignAlgo)
  -> UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
ins </| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

-- | Domain of UTxO
dom :: UTxO hashAlgo dsignAlgo -> Set (TxIn hashAlgo dsignAlgo)
dom (UTxO utxo) = Map.keysSet utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
union
  :: UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
  -> UTxO hashAlgo dsignAlgo
union (UTxO a) (UTxO b) = UTxO $ Map.union a b

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

txup :: Tx hashAlgo dsignAlgo -> Update dsignAlgo
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
scriptStakeCred (ScriptHashObj hs) = Just hs

-- | Computes the set of script hashes required to unlock the transcation inputs
-- and the withdrawals.
scriptsNeeded
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => UTxO hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Set (ScriptHash hashAlgo dsignAlgo)
scriptsNeeded u tx =
  (Set.fromList $ Map.elems $ Map.mapMaybe (getScriptHash . unTxOut) u'')
  `Set.union`
  (Set.fromList $ Maybe.catMaybes $ map (scriptStakeCred . getRwdHK) $ Map.keys withdrawals)
  `Set.union`
  (Set.fromList $ Maybe.catMaybes $ map (scriptStakeCred . cwitness) certificates)
  where unTxOut (TxOut a _) = a
        withdrawals = _wdrls $ _body tx
        UTxO u'' = (txinsScript (txins $ _body tx) u) <| u
        certificates = _certs $ _body tx

-- | Compute the subset of inputs of the set 'txInps' for which each input is
-- locked by a script in the UTxO 'u'.
txinsScript
  :: Set (TxIn hashAlgo dsignAlgo)
  -> UTxO hashAlgo dsignAlgo
  -> Set (TxIn hashAlgo dsignAlgo)
txinsScript txInps (UTxO u) =
  txInps `Set.intersection`
  (Map.keysSet $ Map.filter (\(TxOut a _) ->
                               case a of
                                 AddrBase (ScriptHashObj _) _     -> True
                                 AddrEnterprise (ScriptHashObj _) -> True
                                 _                                -> False) u)
