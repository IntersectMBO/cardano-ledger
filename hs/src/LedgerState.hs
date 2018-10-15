{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('TxWits')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}


module LedgerState
  ( LedgerState(..)
  , asStateTransition
  -- * Genesis State
  , genesisId
  , genesisState
  -- * Validation
  , ValidationError (..)
  ) where

import           Crypto.Hash (hash)
import           Data.List   (find)
import qualified Data.Map    as Map
import           Data.Maybe  (isJust)
import qualified Data.Set    as Set

import           Keys
import           UTxO

-- |Validation errors represent the failures of a transaction to be valid
-- for a given ledger state.
data ValidationError =
                     -- | The transaction inputs are not valid.
                       BadInputs
                     -- | The transaction results in an increased total balance of the ledger.
                     | IncreasedTotalBalance
                     -- | The transaction does not have the required witnesses.
                     | InsuffientWitnesses
                     deriving (Show, Eq)

-- |The validity of a transaction, where an invalid transaction
-- is represented by list of errors.
data Validity = Valid | Invalid [ValidationError] deriving (Show, Eq)

instance Semigroup Validity where
  Valid <> b                 = b
  a <> Valid                 = a
  (Invalid a) <> (Invalid b) = Invalid (a ++ b)

instance Monoid Validity where
  mempty = Valid
  mappend = (<>)

-- |The state associated with a 'Ledger'.
newtype LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo        :: UTxO
  } deriving (Show, Eq)

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [])

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState :: [TxOut] -> LedgerState
genesisState outs = LedgerState
  (UTxO (Map.fromList
    [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs]
  ))

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs :: TxWits -> LedgerState -> Validity
validInputs (TxWits tx _) l =
  if txins tx `Set.isSubsetOf` unspentInputs (getUtxo l)
    then Valid
    else Invalid [BadInputs]
  where unspentInputs (UTxO utxo) = Map.keysSet utxo

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance :: TxWits -> LedgerState -> Validity
preserveBalance (TxWits tx _) l =
  if balance (txouts tx) <= balance (txins tx ◃ getUtxo l)
    then Valid
    else Invalid [IncreasedTotalBalance]

-- |Determine if a transaction input is authorized by a given key.
authTxin :: VKey -> TxIn -> UTxO -> Bool
authTxin key txin (UTxO utxo) =
  case Map.lookup txin utxo of
    Just (TxOut (Addr pay) _) -> hash key == pay
    _                         -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- TODO - should we only check for one witness for each unique input address?
witnessed :: TxWits -> LedgerState -> Validity
witnessed (TxWits tx wits) l =
  if Set.size wits == Set.size ins && all (hasWitness wits) ins
    then Valid
    else Invalid [InsuffientWitnesses]
  where
    utxo = getUtxo l
    ins = inputs tx
    hasWitness witnesses input =
      isJust $ find (isWitness tx input utxo) witnesses
    isWitness tx' input unspent (Wit key sig) =
      verify key tx' sig && authTxin key input unspent

valid :: TxWits -> LedgerState -> Validity
valid tx l =
  validInputs tx l
    <> preserveBalance tx l
    <> witnessed tx l

-- |Apply a raw transaction body as a state transition function on the ledger state.
applyTx :: LedgerState -> Tx -> LedgerState
applyTx ls tx = LedgerState $ txins tx ⋪ getUtxo ls ∪ txouts tx

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition :: LedgerState -> TxWits -> Either [ValidationError] LedgerState
asStateTransition ls tx =
  case valid tx ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyTx ls (body tx)
