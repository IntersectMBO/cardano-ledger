{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Byron.Spec.Ledger.UTxO where

import Byron.Spec.Ledger.Core hiding ((<|))
import Byron.Spec.Ledger.Update (FactorA (..), FactorB (..), PParams (PParams), _factorA, _factorB)
import Data.AbstractSize (HasTypeReps, abstractSize)
import Data.Data (Data)
import Data.Hashable (Hashable)
import qualified Data.Hashable as H
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (typeOf)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

-- | A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId {getTxId :: Hash}
  deriving stock (Show, Generic, Data)
  deriving newtype (Eq, Ord, Hashable, NoThunks)
  deriving anyclass (HasTypeReps)

-- | The input of a UTxO.
--
--      * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural
  deriving
    ( Show
    , Eq
    , Ord
    , Generic
    , Hashable
    , HasTypeReps
    , Data
    , NoThunks
    )

-- | The output of a UTxO.
data TxOut = TxOut
  { addr :: Addr
  , value :: Lovelace
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Generic
    , Hashable
    , HasTypeReps
    , Data
    , NoThunks
    )

-- | The unspent transaction outputs.
newtype UTxO = UTxO
  { unUTxO :: Map TxIn TxOut
  }
  deriving stock (Show, Data)
  deriving newtype (Eq, Relation, Semigroup, Monoid, NoThunks)

-- | Apply function uniformly across all outputs
mapUTxOValues :: (Lovelace -> Lovelace) -> UTxO -> UTxO
mapUTxOValues f (UTxO utxo) = UTxO (f' <$> utxo)
  where
    f' :: TxOut -> TxOut
    f' (TxOut addr value) = TxOut addr (f value)

addValue :: TxOut -> Lovelace -> TxOut
addValue tx@TxOut {value} d = tx {value = value + d}

-- | Construct a UTxO from initial TxOuts
fromTxOuts :: [TxOut] -> UTxO
fromTxOuts = UTxO . Map.fromList . fmap (\out -> (TxIn (mkId out) 0, out))
  where
    mkId = TxId . hash . addr

-- | A raw transaction
data TxBody = TxBody
  { inputs :: [TxIn]
  , outputs :: [TxOut]
  }
  deriving
    ( Eq
    , Show
    , Ord
    , Generic
    , Hashable
    , HasTypeReps
    , Data
    , NoThunks
    )

txid :: TxBody -> TxId
txid = TxId . hash

-- | Total value of a transaction.
txValue :: TxBody -> Lovelace
txValue TxBody {outputs} = sum $ fmap value outputs

-- | Compute the UTxO inputs of a transaction.
txins :: TxBody -> [TxIn]
txins = inputs

-- | Compute the UTxO outputs of a transaction.
txouts :: TxBody -> UTxO
txouts tx =
  UTxO $
    Map.fromList
      [(TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..]]
  where
    transId = txid tx

-- | Determine the total balance contained in the UTxO.
balance :: UTxO -> Lovelace
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where
    addValues b (TxOut _ a) = b <> a

instance Byron.Spec.Ledger.Core.HasHash TxBody where
  hash = Hash . Just . H.hash

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

pcMinFee :: PParams -> Tx -> Lovelace
pcMinFee PParams {_factorA = FactorA a, _factorB = FactorB b} tx =
  fromIntegral $ a + b * txsize tx

txsize :: Tx -> Int
txsize = abstractSize costs
  where
    costs =
      Map.fromList
        [ (typeOf (undefined :: TxIn), 1)
        , (typeOf (undefined :: TxOut), 1)
        , (typeOf (undefined :: Wit), 1)
        ]

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- | Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey (Sig TxBody)
  deriving
    ( Show
    , Eq
    , Ord
    , Generic
    , Hashable
    , HasTypeReps
    , Data
    , NoThunks
    )

-- | A fully formed transaction.
data Tx = Tx
  { body :: TxBody
  , witnesses :: [Wit]
  }
  deriving
    ( Show
    , Eq
    , Generic
    , Hashable
    , HasTypeReps
    , Data
    , NoThunks
    )

instance HasHash [Tx] where
  hash = Hash . Just . H.hash

-- | Create a witness for transaction
makeWitness :: KeyPair -> TxBody -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

makeTxWits :: UTxO -> TxBody -> Tx
makeTxWits (UTxO utxo) tx =
  Tx
    { body = tx
    , witnesses = wits
    }
  where
    getKey txin =
      let TxOut (Addr (VKey o)) _ =
            fromMaybe
              (error "makeTxWits: Missing output for transaction input")
              $ Map.lookup txin utxo
       in KeyPair (SKey o) (VKey o)
    keys = getKey <$> inputs tx
    wits = makeWitness <$> keys <*> pure tx
