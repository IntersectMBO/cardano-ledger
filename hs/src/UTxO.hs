{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : UTxO
Description : Simple UTxO Ledger

This module defines the types and functions for a simple UTxO Ledger
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}

module UTxO
  (
  -- * Primitives
    TxId(..)
  , Addr(..)
  -- * Derived Types
  , TxIn(..)
  , TxOut(..)
  , UTxO(..)
  , Tx(..)
  -- * Functions
  , txid
  , txins
  , txouts
  , balance
  , (<|)
  , (</|)
  -- , verify
  , union
  , makeWitness
  , Wit(..)
  , TxWits(..)
  ) where

import           Crypto.Hash             (Digest, SHA256, hash)
import qualified Data.ByteArray          as BA
import qualified Data.ByteString.Char8   as BS
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Numeric.Natural         (Natural)

import           Coin                    (Coin (..))
import           Keys

import           Delegation.Certificates (DCert (..))

-- |A hash
type Hash = Digest SHA256

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving (Show, Eq, Ord)

-- |An address for UTxO.
data Addr = AddrTxin HashKey HashKey
          deriving (Show, Eq, Ord)

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural deriving (Show, Eq, Ord)

-- |The output of a UTxO.
data TxOut = TxOut Addr Coin deriving (Show, Eq, Ord)

-- |The unspent transaction outputs.
newtype UTxO = UTxO (Map TxIn TxOut) deriving (Show, Eq, Ord)

-- |A raw transaction
data Tx = Tx { inputs  :: !(Set TxIn)
             , outputs :: [TxOut]
             , certs   :: !(Set DCert)
             } deriving (Show, Eq, Ord)

-- |Compute the id of a transaction.
txid :: Tx -> TxId
txid = TxId . hash

-- |Compute the UTxO inputs of a transaction.
txins :: Tx -> Set TxIn
txins = inputs

-- |Compute the transaction outputs of a transaction.
txouts :: Tx -> UTxO
txouts tx = UTxO $
  Map.fromList [(TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0..]]
  where
    transId = txid tx

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey !(Sig Tx) deriving (Show, Eq, Ord)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits = TxWits
              { body       :: !Tx
              , witnessSet :: !(Set Wit)
              } deriving (Show, Eq, Ord)

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

-- |Domain restriction
(<|) :: Set TxIn -> UTxO -> UTxO
ins <| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.member` ins) utxo

-- |Domain exclusion
(</|) :: Set TxIn -> UTxO -> UTxO
ins </| (UTxO utxo) =
  UTxO $ Map.filterWithKey (\k _ -> k `Set.notMember` ins) utxo

-- |Combine two collections of UTxO.
--
--     * TODO - Should we return 'Maybe UTxO' so that we can return
-- Nothing when the collections are not disjoint?
union :: UTxO -> UTxO -> UTxO
union (UTxO a) (UTxO b) = UTxO $ Map.union a b

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Coin
balance (UTxO utxo) = foldr addCoins mempty utxo
  where addCoins (TxOut _ a) b = a <> b

instance BA.ByteArrayAccess Tx where
  length        = BA.length . BS.pack . show
  withByteArray = BA.withByteArray . BS.pack  . show
