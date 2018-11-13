{-# LANGUAGE BangPatterns #-}

{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('TxWits')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}


module LedgerState
  ( LedgerState(..)
  , RewardAcnt(..)
  , mkRwdAcnt
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

import           Crypto.Hash             (hash)
import           Data.List               (find)
import qualified Data.Map                as Map
import           Data.Maybe              (isJust, mapMaybe)
import qualified Data.Set                as Set

import           Coin                    (Coin (..))
import           Slot                    (Slot (..), Epoch (..))
import           Keys
import           UTxO

import           Delegation.Certificates (DCert (..))
import           Delegation.StakePool    (Delegation (..), StakePool (..))

-- | A ledger consists of a list of entries where each such entry is either a
-- stake delegation step or a transaction.

data LedgerEntry =
    TransactionData !TxWits
  | DelegationData !DCert
    deriving (Show, Eq)

type Ledger = [LedgerEntry]

data LedgerValidation = LedgerValidation [ValidationError] LedgerState

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

-- |An account based address for a rewards
newtype RewardAcnt = RewardAcnt HashKey
  deriving (Show, Eq, Ord)

mkRwdAcnt :: KeyPair -> RewardAcnt
mkRwdAcnt keys = RewardAcnt $ hashKey $ vKey keys

-- |The state associated with the current stake delegation.
data DelegationState =
    DelegationState
    {
    -- |The active accounts.
      getAccounts    :: Map.Map RewardAcnt Coin
    -- |The active stake keys.
    , getStKeys      :: Map.Map HashKey Slot
    -- |The current delegations.
    , getDelegations :: Map.Map HashKey HashKey
    -- |The active stake pools.
    , getStPools     :: Map.Map HashKey (StakePool, Slot)
    -- |A map of retiring stake pools to the epoch when they retire.
    , getRetiring    :: Map.Map HashKey Epoch
    } deriving (Show, Eq)

emptyDelegation :: DelegationState
emptyDelegation =
    DelegationState Map.empty Map.empty Map.empty Map.empty Map.empty

-- |The state associated with a 'Ledger'.
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo            :: !UTxO
    -- |The current delegation state
  , getDelegationState :: !DelegationState
    -- |The current epoch.
  , getEpoch           :: Epoch
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
  (Epoch 0)

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
    Just (TxOut (AddrTxin pay _) _) -> hashKey key == pay
    _                               -> False

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- We check that there are not more witnesses than inputs, if several inputs
-- from the same address are used, it is not strictly necessary to include more
-- than one witness.
witnessed :: TxWits -> LedgerState -> Validity
witnessed (TxWits tx wits) l =
  if Set.size wits <= Set.size ins && all (hasWitness wits) ins
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
validKeyRegistration :: DCert -> LedgerState -> Validity
validKeyRegistration cert (LedgerState _ ds _) =
  case cert of
    RegKey key -> if not $ Map.member (hashKey key) (getStKeys ds)
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
    _          -> Valid

validKeyDeregistration :: DCert -> LedgerState -> Validity
validKeyDeregistration cert (LedgerState _ ds _) =
  case cert of
    DeRegKey key -> if Map.member (hashKey key) (getStKeys ds)
                    then Valid else Invalid [StakeKeyNotRegistered]
    _            -> Valid

validStakeDelegation :: DCert -> LedgerState -> Validity
validStakeDelegation cert (LedgerState _ ds _) =
  case cert of
    Delegate (Delegation source target)
      -> if Map.member (hashKey source) (getStKeys ds) &&
            Map.member (hashKey target) (getStPools ds)
         then Valid else Invalid [StakeDelegationImpossible]
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister :: DCert -> LedgerState -> Validity
validStakePoolRegister _ _ = Valid

validStakePoolRetire :: DCert -> LedgerState -> Validity
validStakePoolRetire cert (LedgerState _ ds _) =
  case cert of
    RetirePool key _ -> if Map.member (hashKey key) (getStPools ds)
                        then Valid else Invalid [StakePoolNotRegisteredOnKey]
    _                -> Valid

validDelegation :: DCert -> LedgerState -> Validity
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
  :: Slot -> LedgerState -> LedgerEntry -> Either [ValidationError] LedgerState
asStateTransition _ ls (TransactionData tx) =
  case valid tx ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyTx ls (body tx)

asStateTransition slot ls (DelegationData cert) =
  case validDelegation cert ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyDCert slot cert ls

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: Slot -> LedgerValidation -> LedgerEntry -> LedgerValidation
asStateTransition' _ (LedgerValidation valErrors ls) (TransactionData tx) =
    let ls' = applyTx ls (body tx) in
    case valid tx ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

asStateTransition' slot (LedgerValidation valErrors ls) (DelegationData cert) =
    let ls' = applyDCert slot cert ls in
    case validDelegation cert ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Epoch -> LedgerState
retirePools ls@(LedgerState _ ds _) epoch = ls
    { getDelegationState = ds
      { getStPools =
          Map.filterWithKey
           (\hk _ -> Map.notMember hk retiring)
           (getStPools ds)
      , getRetiring = active }
    }
  where (active, retiring) = Map.partition (epoch /=) (getRetiring ds)

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: LedgerState -> Tx -> LedgerState
applyTxBody ls tx = ls { getUtxo = newUTxOs }
  where newUTxOs = txins tx </| getUtxo ls `union` txouts tx

-- |Apply a delegation certificate as a state transition function on the ledger state.
applyDCert :: Slot -> DCert -> LedgerState -> LedgerState
applyDCert slot (RegKey key) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStKeys = Map.insert hksk slot (getStKeys ds)
    , getAccounts = Map.insert (RewardAcnt hksk) (Coin 0) (getAccounts ds)}
  }
  where hksk = hashKey key

applyDCert _ (DeRegKey key) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStKeys = Map.delete hksk (getStKeys ds)
    , getAccounts = Map.delete (RewardAcnt hksk) (getAccounts ds)
    , getDelegations = Map.delete hksk (getDelegations ds) }
  }
  where hksk = hashKey key

-- TODO do we also have to check hashKey target?
applyDCert _ (Delegate (Delegation source target)) ls@(LedgerState _ ds _) = ls
  {getDelegationState = ds
    { getDelegations =
        Map.insert (hashKey source) (hashKey target) (getDelegations ds)}
  }

-- TODO what happens if there's already a pool registered with that key?
applyDCert slot (RegPool sp) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStPools = Map.insert hsk (sp, slot') pools
    , getRetiring = Map.delete hsk (getRetiring ds)}}
  where hsk = hashKey $ poolPubKey sp
        pools = getStPools ds
        slot' = case Map.lookup hsk pools of
                  Just (_, s) -> s
                  Nothing -> slot

-- TODO check epoch (not in new doc atm.)
applyDCert _ (RetirePool key epoch) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getRetiring = retiring}}
  where retiring = Map.insert hk_sp epoch (getRetiring ds)
        hk_sp = hashKey key

-- |Apply an ordered collection of certificates as a state transition function
-- on the ledger state.
applyDCerts :: Slot -> LedgerState -> Set.Set DCert -> LedgerState
applyDCerts slot = Set.fold (applyDCert slot)

-- |Apply a transaction as a state transition function on the ledger state.
applyTransaction :: Slot -> LedgerState -> TxWits -> LedgerState
applyTransaction slot ls tx = applyTxBody (applyDCerts slot ls cs) (body tx)
  where cs = (certs . body) tx

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map.Map HashKey Coin
delegatedStake ls@(LedgerState _ ds _) = Map.fromListWith mappend delegatedOutputs
  where
    getOutputs (UTxO utxo) = Map.elems utxo
    addStake delegations (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup hsk delegations
      return (pool, c)
    outs = getOutputs . getUtxo $ ls
    delegatedOutputs = mapMaybe (addStake (getDelegations ds)) outs
