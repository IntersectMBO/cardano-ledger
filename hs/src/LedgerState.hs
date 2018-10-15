{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('TxWits')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}


module LedgerState
  ( LedgerState(..)
  -- * state transitions
  , applyTransaction
  , asStateTransition
  , delegatedStake
  , retirePools
  -- * Genesis State
  , genesisId
  , genesisState
  -- * Validation
  , ValidationError (..)
  ) where

import           Crypto.Hash (hash)
import           Data.List   (find)
import qualified Data.Map    as Map
import           Data.Maybe  (isJust, mapMaybe)
import qualified Data.Set    as Set

import           Coin        (Coin(..))
import           Keys
import           UTxO

import           Delegation.Certificates (Cert(..))
import           Delegation.StakePool (Delegation(..), StakePool(..))
    
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
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo        :: UTxO
    -- |The active accounts.
  , getAccounts    :: Map.Map HashKey Coin
    -- |The active stake keys.
  , getStKeys      :: Set.Set HashKey
    -- |The current delegations.
  , getDelegations :: Map.Map HashKey HashKey
    -- |The active stake pools.
  , getStPools     :: Set.Set HashKey
    -- |A map of retiring stake pools to the epoch when they retire.
  , getRetiring    :: Map.Map HashKey Int
    -- |The current epoch.
  , getEpoch       :: Int
  } deriving (Show, Eq)

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [] Set.empty)

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState :: [TxOut] -> LedgerState
genesisState outs = LedgerState
  (UTxO (Map.fromList
    [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs]
  ))
  Map.empty
  Set.empty
  Map.empty
  Set.empty
  Map.empty
  0

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
    Just (TxOut (AddrTxin pay _) _) -> hash key == pay
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
applyTx ls tx =
    LedgerState (txins tx ⋪ getUtxo ls ∪ txouts tx)
                (getAccounts ls)
                (getStKeys ls)
                (getDelegations ls)
                (getStPools ls)
                (getRetiring ls)
                (getEpoch ls)

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition :: LedgerState -> TxWits -> Either [ValidationError] LedgerState
asStateTransition ls tx =
  case valid tx ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyTx ls (body tx)


-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Int -> LedgerState
retirePools ls epoch = ls
  { getStPools = Set.difference (getStPools ls) (Map.keysSet retiring)
  , getRetiring = active }
  where (active, retiring) = Map.partition ((/=) epoch) (getRetiring ls)

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: LedgerState -> Tx -> LedgerState
applyTxBody ls tx = ls { getUtxo = newUTxOs }
  where newUTxOs = (txins tx ⋪ (getUtxo ls) ∪ txouts tx)

-- |Apply a certificate as a state transition function on the ledger state.
applyCert :: Cert -> LedgerState -> LedgerState
applyCert (RegKey key) ls = ls
  { getStKeys = (Set.insert (hashKey key) (getStKeys ls))
  , getAccounts = (Map.insert (hashKey key) (Coin 0) (getAccounts ls))}
applyCert (DeRegKey key) ls = ls
  { getStKeys = (Set.delete (hashKey key) (getStKeys ls))
  , getAccounts = Map.delete (hashKey key) (getAccounts ls)
  , getDelegations = Map.delete (hashKey key) (getDelegations ls)
    }
applyCert (Delegate (Delegation source target)) ls =
  ls {getDelegations =
    Map.insert (hashKey source) (hashKey target) (getDelegations ls)}
applyCert (RegPool sp) ls = ls
  { getStPools = (Set.insert hsk (getStPools ls))
  , getRetiring = Map.delete hsk (getRetiring ls)}
  where hsk = hashKey $ poolPubKey sp
applyCert (RetirePool key epoch) ls = ls {getRetiring = retiring}
  where retiring = Map.insert (hashKey key) epoch (getRetiring ls)

-- |Apply a collection of certificates as a state transition function on
-- the ledger state.
applyCerts :: LedgerState -> Set.Set Cert -> LedgerState
applyCerts = Set.fold applyCert

-- |Apply a transaction as a state transition function on the ledger state.
applyTransaction :: LedgerState -> TxWits -> LedgerState
applyTransaction ls tx = applyTxBody (applyCerts ls cs) (body tx)
  where cs = (certs . body) tx

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map.Map HashKey Coin
delegatedStake ls = Map.fromListWith mappend delegatedOutputs
  where
    getOutputs (UTxO utxo) = Map.elems utxo
    addStake delegations (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup (HashKey hsk) delegations
      return (pool, c)
    addStake _ _ = Nothing
    outs = getOutputs . getUtxo $ ls
    delegatedOutputs = mapMaybe (addStake (getDelegations ls)) outs
