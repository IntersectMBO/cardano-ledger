{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('TxWits')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}


module LedgerState
  ( LedgerState(..)
  , DelegationState(..)
  , Ledger
  , LedgerEntry(..)
  -- * state transitions
  , applyTransaction
  , asStateTransition
  , delegatedStake
  , retirePools
  , emptyDelegation
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
import           Numeric.Natural       (Natural)

import           Coin        (Coin(..))
import           Keys
import           UTxO

import           Delegation.Certificates (Cert(..))
import           Delegation.StakePool (Delegation(..), StakePool(..))

-- | A ledger consists of a list of entries where each such entry is either a
-- stake delegation step or a transaction.

data LedgerEntry =
    TransactionData TxWits
  | DelegationData Cert
    deriving (Show, Eq)

type Ledger = [LedgerEntry]

-- |Validation errors represent the failures of a transaction to be valid
-- for a given ledger state.
data ValidationError =
  -- | The transaction inputs are not valid.
    BadInputs
  -- | The transaction results in an increased total balance of the ledger.
  | IncreasedTotalBalance
  -- | The transaction does not have the required witnesses.
  | InsuffientWitnesses
  -- | A stake key cannot be registered again.
  | StakeKeyAlreadyRegistered
  -- | A stake key must be registered to be used or deregistered.
  | StakeKeyNotRegistered
  -- | The stake key to which is delegated is not known.
  | StakeDelegationImpossible
  -- | Stake pool not registered for key, cannot be retired.
  | StakePoolNotRegisteredOnKey
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

-- |The state associated with the current stake delegation.
data DelegationState =
    DelegationState
    {
    -- |The active accounts.
      getAccounts    :: Map.Map HashKey Coin
    -- |The active stake keys.
    , getStKeys      :: Set.Set HashKey
    -- |The current delegations.
    , getDelegations :: Map.Map HashKey HashKey
    -- |The active stake pools.
    , getStPools     :: Set.Set HashKey -- TODO in doc its a map to Cert
    -- |A map of retiring stake pools to the epoch when they retire.
    , getRetiring    :: Map.Map HashKey Natural
    } deriving (Show, Eq)

emptyDelegation :: DelegationState
emptyDelegation =
    DelegationState Map.empty Set.empty Map.empty Set.empty Map.empty

-- |The state associated with a 'Ledger'.
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo            :: UTxO
    -- |The current delegation state
  , getDelegationState :: DelegationState
    -- |The current epoch.
  , getEpoch           :: Natural
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
  emptyDelegation
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
  if balance (txouts tx) <= balance (txins tx <| getUtxo l)
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

-- The rules for checking validiy of stake delegation transitions return
-- `certificate_type_correct(cert) -> valid_cert(cert)`, i.e., if the
-- certificate is of a different type, it's considered to be valid due to the
-- falsified hypothesis.

-- | Checks whether a key registration certificat is valid.
validKeyRegistration :: Cert -> LedgerState -> Validity
validKeyRegistration cert (LedgerState _ ds _) =
  case cert of
    RegKey key -> if not $ Set.member (hashKey key) (getStKeys ds)
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
    _          -> Valid

validKeyDeregistration :: Cert -> LedgerState -> Validity
validKeyDeregistration cert (LedgerState _ ds _) =
  case cert of
    DeRegKey key -> if Set.member (hashKey key) (getStKeys ds)
                    then Valid else Invalid [StakeKeyNotRegistered]
    _            -> Valid

validStakeDelegation :: Cert -> LedgerState -> Validity
validStakeDelegation cert (LedgerState _ ds _) =
  case cert of
    Delegate (Delegation source target)
      -> if Set.member (hashKey source) (getStKeys ds) &&
            Set.member (hashKey target) (getStPools ds)
         then Valid else Invalid [StakeDelegationImpossible]
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister :: Cert -> LedgerState -> Validity
validStakePoolRegister _ _ = Valid

validStakePoolRetire :: Cert -> LedgerState -> Validity
validStakePoolRetire cert (LedgerState _ ds _) =
  case cert of
    RetirePool key _ -> if Set.member (hashKey key) (getStPools ds)
                        then Valid else Invalid [StakePoolNotRegisteredOnKey]
    _                -> Valid

validDelegation :: Cert -> LedgerState -> Validity
validDelegation cert l =
     validKeyRegistration cert l
  <> validKeyDeregistration cert l
  <> validStakeDelegation cert l
  <> validStakePoolRegister cert l
  <> validStakePoolRetire cert l

-- |Apply a raw transaction body as a state transition function on the ledger state.
applyTx :: LedgerState -> Tx -> LedgerState
applyTx ls tx =
    LedgerState (txins tx </| getUtxo ls `union` txouts tx)
                (getDelegationState ls)
                (getEpoch ls)

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition
  :: LedgerState -> LedgerEntry -> Either [ValidationError] LedgerState
asStateTransition ls (TransactionData tx) =
  case valid tx ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyTx ls (body tx)

asStateTransition ls (DelegationData cert) =
  case validDelegation cert ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyCert cert ls

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Natural -> LedgerState
retirePools ls@(LedgerState _ ds _) epoch = ls
    { getDelegationState = ds
      { getStPools = Set.difference (getStPools ds) (Map.keysSet retiring)
      , getRetiring = active }
    }
  where (active, retiring) = Map.partition ((/=) epoch) (getRetiring ds)

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: LedgerState -> Tx -> LedgerState
applyTxBody ls tx = ls { getUtxo = newUTxOs }
  where newUTxOs = (txins tx </| (getUtxo ls) `union` txouts tx)

-- |Apply a certificate as a state transition function on the ledger state.
applyCert :: Cert -> LedgerState -> LedgerState
applyCert (RegKey key) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStKeys = (Set.insert hk_sk (getStKeys ds))
    , getAccounts = (Map.insert hk_sk (Coin 0) (getAccounts ds))}
  }
  where hk_sk = hashKey key

applyCert (DeRegKey key) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStKeys = Set.delete hk_sk (getStKeys ds)
    , getAccounts = Map.delete hk_sk (getAccounts ds)
    , getDelegations = Map.delete hk_sk (getDelegations ds) }
  }
  where hk_sk = hashKey key

-- TODO do we also have to check hashKey target?
applyCert (Delegate (Delegation source target)) ls@(LedgerState _ ds _) = ls
  {getDelegationState = ds
    { getDelegations =
        Map.insert (hashKey source) (hashKey target) (getDelegations ds)}
  }

-- TODO what happens if there's already a pool registered with that key?
applyCert (RegPool sp) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStPools = Set.insert hsk (getStPools ds)
    , getRetiring = Map.delete hsk (getRetiring ds)}}
  where hsk = hashKey $ poolPubKey sp

-- TODO check epoch (not in new doc atm.)
applyCert (RetirePool key epoch) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getRetiring = retiring}}
  where retiring = Map.insert hk_sp epoch (getRetiring ds)
        hk_sp = hashKey key

-- |Apply an ordered collection of certificates as a state transition function
-- on the ledger state.
applyCerts :: LedgerState -> Set.Set Cert -> LedgerState
applyCerts = Set.fold applyCert

-- |Apply a transaction as a state transition function on the ledger state.
applyTransaction :: LedgerState -> TxWits -> LedgerState
applyTransaction ls tx = applyTxBody (applyCerts ls cs) (body tx)
  where cs = (certs . body) tx

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map.Map HashKey Coin
delegatedStake ls@(LedgerState _ ds _) = Map.fromListWith mappend delegatedOutputs
  where
    getOutputs (UTxO utxo) = Map.elems utxo
    addStake delegations (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup (HashKey hsk) delegations
      return (pool, c)
    addStake _ _ = Nothing
    outs = getOutputs . getUtxo $ ls
    delegatedOutputs = mapMaybe (addStake (getDelegations ds)) outs
