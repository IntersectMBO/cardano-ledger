{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ledger.UTxO where

import Data.AbstractSize (HasTypeReps, typeReps, abstractSize)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence ((<|), empty)
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

import Ledger.Core hiding ((<|))
import Ledger.Update (PParams (PParams), _factorA, _factorB)

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Hashable)

instance HasTypeReps TxId where
  typeReps x = typeOf x <| typeOf (getTxId x) <| empty

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn id = TxIn id Natural deriving (Show, Eq, Ord, Generic, Hashable)

instance HasTypeReps i => HasTypeReps (TxIn i) where
  typeReps x@(TxIn i' n) = typeOf x <| typeOf i' <| typeOf n <| empty

-- |The output of a UTxO.
data TxOut = TxOut { addr  :: Addr
                   , value :: Lovelace
                   } deriving (Show, Eq, Ord, Generic, Hashable)

-- |The unspent transaction outputs.
newtype UTxO id = UTxO
  { unUTxO :: Map (TxIn id) TxOut
  } deriving stock (Show)
    deriving newtype (Eq, Relation)

addValue :: TxOut -> Lovelace -> TxOut
addValue tx@TxOut{ value } d = tx { value = value + d }

-- | Construct a UTxO from initial TxOuts, using the supplied id generator
fromTxOuts :: Ord id => (TxOut -> id) -> [TxOut] -> UTxO id
fromTxOuts mkId = UTxO . Map.fromList . fmap (\out -> (TxIn (mkId out) 0, out))

-- | Construct a UTxO from initial TxOuts, using Owner as TxId
fromTxOutsNatural :: [TxOut] -> UTxO Natural
fromTxOutsNatural = fromTxOuts (unOwner . owner . addr)

-- |A raw transaction
data Tx id = Tx { txid    :: id
                , inputs  :: [TxIn id]
                , outputs :: [TxOut]
                } deriving (Show, Ord, Generic, Hashable)

instance HasTypeReps i => HasTypeReps (Tx i) where
  typeReps x@(Tx i' inputs outputs)
    = typeOf x <| typeOf i' <| typeOf inputs <| typeOf outputs <| empty

instance Eq id => Eq (Tx id) where
  (Tx _ ins outs) == (Tx _ ins' outs') = ins == ins' && outs == outs'

-- | Total value of a transaction.
txValue :: Tx id -> Lovelace
txValue Tx { outputs } = sum $ fmap value outputs

-- |Compute the UTxO inputs of a transaction.
txins :: Tx id -> [TxIn id]
txins = inputs

-- |Compute the UTxO outputs of a transaction.
txouts :: Ord id => Tx id -> UTxO id
txouts tx = UTxO $ Map.fromList
  [ (TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..] ]
  where transId = txid tx

-- |Determine the total balance contained in the UTxO.
balance :: UTxO id -> Lovelace
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where addValues b (TxOut _ a) = b <> a

instance Ledger.Core.HasHash (Tx TxId) where
  hash = Hash . H.hash

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

pcMinFee :: forall id . HasTypeReps id => PParams -> Tx id -> Lovelace
pcMinFee PParams {_factorA = a, _factorB = b} tx
  = fromIntegral $ a * txsize tx + b

txsize :: forall id . HasTypeReps id => Tx id -> Int
txsize = abstractSize costs
  where costs = Map.fromList [ (typeOf (undefined :: TxIn id), 1)
                             , (typeOf (undefined :: TxOut)  , 1)
                             ]


---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit id = Wit VKey (Sig (Tx id)) deriving (Show, Eq, Ord, Generic, Hashable)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits id = TxWits
                { body      :: Tx id
                , witnesses :: [Wit id]
                } deriving (Show, Eq, Generic, Hashable)

instance HasTypeReps i => HasTypeReps (TxWits i) where
  typeReps (TxWits b w) = typeOf b <| typeOf w <| empty

instance (Show id, Hashable id) => HasHash [TxWits id] where
  hash = Hash . H.hash

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx id -> Wit id
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

makeTxWits :: Ord id => UTxO id -> Tx id -> TxWits id
makeTxWits (UTxO utxo) tx = TxWits
  { body      = tx
  , witnesses = wits
  }
 where
  getKey txin =
    let
      TxOut (Addr (VKey o)) _ =
        fromMaybe
            (error "makeTxWits: Missing output for transaction input")
          $ Map.lookup txin utxo
    in KeyPair (SKey o) (VKey o)
  keys = getKey <$> inputs tx
  wits = makeWitness <$> keys <*> pure tx
