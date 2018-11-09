{-# LANGUAGE FlexibleContexts #-}
{- |
Core implementation of the ledger logic.
-}
module Ledger.Abstract where

import qualified Data.ByteString.Char8 as BS
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import UTxO

-- | Hash part of the ledger paylod
class HasHash a where
  hash :: a -> Hash

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving (Show, Eq, Ord)

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut = TxOut Addr Value deriving (Show, Eq)

-- |The unspent transaction outputs.
newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq)

-- |A raw transaction
data Tx = Tx { inputs  :: Set TxIn
             , outputs :: [TxOut]
             } deriving (Show, Eq)

-- |Compute the id of a transaction.
txid :: HasHash Tx => Tx -> TxId
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: Tx -> Set TxIn
txins = inputs

-- |Compute the UTxO outputs of a transaction.
txouts :: HasHash Tx => Tx -> UTxO
txouts tx = UTxO $ Map.fromList
  [ (TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..] ]
  where transId = txid tx

txfee :: Tx -> Value
txfee = undefined

-- |Domain restriction
--
--     * __TODO__ - better symbol?
(◁) :: Set TxIn -> UTxO -> UTxO
ins ◁ (UTxO utxo) = UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion
--
(⋪) :: Set TxIn -> UTxO -> UTxO
ins ⋪ (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
(∪) :: UTxO -> UTxO -> UTxO
(UTxO a) ∪ (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Value
balance (UTxO utxo) = foldl' addValues mempty utxo
  where addValues b (TxOut _ a) = b <> a

---------------------------------------------------------------------------------
-- Signing and verification
---------------------------------------------------------------------------------

-- |Representation of the owner of key pair.
newtype Owner = Owner Natural deriving (Show, Eq, Ord)

-- |Signing Key.
newtype SKey = SKey Owner deriving (Show, Eq, Ord)

-- |Verification Key.
newtype VKey = VKey Owner deriving (Show, Eq, Ord)

-- |Key Pair.
data KeyPair = KeyPair
  {sKey :: SKey, vKey :: VKey} deriving (Show, Eq, Ord)

-- |Return a key pair for a given owner.
keyPair :: Owner -> KeyPair
keyPair owner = KeyPair (SKey owner) (VKey owner)

-- |A digital signature.
data Sig a = Sig a Owner deriving (Show, Eq, Ord)

-- |Produce a digital signature
sign :: SKey -> a -> Sig a
sign (SKey k) d = Sig d k

-- |Verify a digital signature
verify :: Eq a => VKey -> a -> Sig a -> Bool
verify (VKey vk) vd (Sig sd sk) = vk == sk && vd == sd

---------------------------------------------------------------------------------
-- Transaction witnesses
---------------------------------------------------------------------------------

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey (Sig Tx) deriving (Show, Eq)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits = TxWits
              { body     :: Tx
              , witneses :: Set Wit
              } deriving (Show, Eq)

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

