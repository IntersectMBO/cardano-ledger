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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Ledger.UTxO where

import           Control.Monad (replicateM)
import           Data.AbstractSize (HasTypeReps, abstractSize)
import           Data.Data (Data, Typeable)
import           Data.Hashable (Hashable)
import qualified Data.Hashable as H
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Typeable (typeOf)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ledger.Core hiding ((<|))
import           Ledger.Update (FactorA (..), FactorB (..), PParams (PParams), _factorA, _factorB)

import           Test.Goblin (AddShrinks (..), GeneOps (..), Goblin (..), SeedGoblin (..), TinkerM,
                     saveInBagOfTricks, tinkerRummagedOrConjureOrSave, (<$$>))
import           Test.Goblin.TH (deriveAddShrinks, deriveGoblin, deriveSeedGoblin)

-- |A unique ID of a transaction, which is computable from the transaction.
newtype TxId = TxId { getTxId :: Hash }
  deriving stock (Show, Generic, Data, Typeable)
  deriving newtype (Eq, Ord, Hashable)
  deriving anyclass (HasTypeReps)

-- |The input of a UTxO.
--
--     * __TODO__ - is it okay to use list indices instead of implementing the Ix Type?
data TxIn = TxIn TxId Natural
  deriving (Show, Eq, Ord, Generic, Hashable, HasTypeReps, Data, Typeable)

-- |The output of a UTxO.
data TxOut = TxOut { addr  :: Addr
                   , value :: Lovelace
                   } deriving (Show, Eq, Ord, Generic, Hashable, HasTypeReps, Data, Typeable)

-- |The unspent transaction outputs.
newtype UTxO = UTxO
  { unUTxO :: Map TxIn TxOut
  } deriving stock (Show, Data, Typeable)
    deriving newtype (Eq, Relation, Semigroup, Monoid)

-- | Apply function uniformly across all outputs
mapUTxOValues :: (Lovelace -> Lovelace) -> UTxO -> UTxO
mapUTxOValues f (UTxO utxo) = UTxO (f' <$> utxo)
  where
    f' :: TxOut -> TxOut
    f' (TxOut addr value) = TxOut addr (f value)

addValue :: TxOut -> Lovelace -> TxOut
addValue tx@TxOut{ value } d = tx { value = value + d }

-- | Construct a UTxO from initial TxOuts
fromTxOuts :: [TxOut] -> UTxO
fromTxOuts = UTxO . Map.fromList . fmap (\out -> (TxIn (mkId out) 0, out))
  where mkId = TxId . hash . addr

-- | A raw transaction
data Tx = Tx
  { inputs  :: [TxIn]
  , outputs :: [TxOut]
  } deriving (Eq, Show, Ord, Generic, Hashable, HasTypeReps, Data, Typeable)

txid :: Tx -> TxId
txid = TxId . hash

-- | Total value of a transaction.
txValue :: Tx -> Lovelace
txValue Tx { outputs } = sum $ fmap value outputs

-- |Compute the UTxO inputs of a transaction.
txins :: Tx -> [TxIn]
txins = inputs

-- |Compute the UTxO outputs of a transaction.
txouts :: Tx -> UTxO
txouts tx = UTxO $ Map.fromList
  [ (TxIn transId idx, out) | (out, idx) <- zip (outputs tx) [0 ..] ]
  where transId = txid tx

-- |Determine the total balance contained in the UTxO.
balance :: UTxO -> Lovelace
balance (UTxO utxo) = Map.foldl' addValues mempty utxo
  where addValues b (TxOut _ a) = b <> a

instance Ledger.Core.HasHash Tx where
  hash = Hash . Just . H.hash

---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

pcMinFee :: PParams -> Tx -> Lovelace
pcMinFee PParams {_factorA = FactorA a, _factorB = FactorB b} tx
  = fromIntegral $ a + b * txsize tx

txsize :: Tx -> Int
txsize = abstractSize costs
  where costs = Map.fromList [ (typeOf (undefined :: TxIn) , 1)
                             , (typeOf (undefined :: TxOut), 1)
                             ]


---------------------------------------------------------------------------------
-- UTxO transitions
---------------------------------------------------------------------------------

-- |Proof/Witness that a transaction is authorized by the given key holder.
data Wit = Wit VKey (Sig Tx)
  deriving (Show, Eq, Ord, Generic, Hashable, HasTypeReps, Data, Typeable)

-- |A fully formed transaction.
--
--     * __TODO__ - Would it be better to name this type Tx, and rename Tx to TxBody?
data TxWits = TxWits
  { body      :: Tx
  , witnesses :: [Wit]
  } deriving (Show, Eq, Generic, Hashable, HasTypeReps, Data, Typeable)

instance HasHash [TxWits] where
  hash = Hash . Just . H.hash

-- |Create a witness for transaction
makeWitness :: KeyPair -> Tx -> Wit
makeWitness keys tx = Wit (vKey keys) (sign (sKey keys) tx)

makeTxWits :: UTxO -> Tx -> TxWits
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


--------------------------------------------------------------------------------
-- Goblins instances
--------------------------------------------------------------------------------

deriveGoblin ''TxIn
deriveGoblin ''TxOut
deriveGoblin ''TxWits
deriveGoblin ''Wit
deriveGoblin ''TxId

instance GeneOps g => Goblin g Tx where
  tinker gen = do
    fIs <- fillEmptyList
    fOs <- fillEmptyList
    is <- tinkerRummagedOrConjureOrSave
            (fIs <$$>
              (tinker ((\(Tx x _) -> x) <$> gen)))
    os <- tinkerRummagedOrConjureOrSave
            (fOs <$$>
              (tinker ((\(Tx _ x) -> x) <$> gen)))
    tinkerRummagedOrConjureOrSave
      (pure (Tx <$> is <*> os))
   where
    -- This function will insert a conjured value to an empty list. We can
    -- thus use it to ensure that the `txIns` and `txOuts` will never be
    -- empty.
    fillEmptyList :: Goblin g a => TinkerM g ([a] -> [a])
    fillEmptyList = do
      v <- conjure
      pure (\xs -> case xs of
                     [] -> [v]
                     _  -> xs)

  conjure = saveInBagOfTricks =<< do
    -- Ensure that these lists are never empty.
    listLenI <- (+1) <$> transcribeGenesAsInt 15
    listLenO <- (+1) <$> transcribeGenesAsInt 15
    inputs <- replicateM listLenI conjure
    outputs <- replicateM listLenO conjure
    pure (Tx inputs outputs)

--------------------------------------------------------------------------------
-- AddShrinks instances
--------------------------------------------------------------------------------

deriveAddShrinks ''Tx
deriveAddShrinks ''TxId
deriveAddShrinks ''TxIn
deriveAddShrinks ''TxOut
deriveAddShrinks ''TxWits
deriveAddShrinks ''Wit


--------------------------------------------------------------------------------
-- SeedGoblin instances
--------------------------------------------------------------------------------

deriveSeedGoblin ''UTxO
deriveSeedGoblin ''TxId
deriveSeedGoblin ''TxIn
deriveSeedGoblin ''TxOut
