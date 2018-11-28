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
  , LedgerValidation(..)
  , KeyPairs
  -- * state transitions
  , asStateTransition
  , asStateTransition'
  , delegatedStake
  , retirePools
  , emptyDelegation
  -- * Genesis State
  , genesisId
  , genesisState
  -- * Validation
  , ValidationError (..)
  ) where

import           Control.Monad           (foldM)
import           Crypto.Hash             (hash)
import           Data.List               (find)
import qualified Data.Map                as Map
import           Data.Maybe              (isJust, mapMaybe, fromMaybe)
import qualified Data.Set                as Set

import           Coin                    (Coin (..))
import           Slot                    (Slot (..), Epoch (..), (-*))
import           Keys
import           UTxO
import           PrtlConsts              (PrtlConsts(..))

import           Delegation.Certificates (DCert (..), refund)
import           Delegation.StakePool    (Delegation (..), StakePool (..))

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs = [(KeyPair, KeyPair)]

-- | A ledger validation state consists of a ledger state 't' and the list of
-- validation errors that occurred from a valid 's' to reach 't'.
data LedgerValidation = LedgerValidation [ValidationError] LedgerState
                        deriving (Show, Eq)

-- |Validation errors represent the failures of a transaction to be valid
-- for a given ledger state.
data ValidationError =
  -- | The transaction inputs are not valid.
    BadInputs
  -- | The transaction has expired
  | Expired Slot Slot
  -- | The transaction fee is too small
  | FeeTooSmall Coin Coin
  -- | Value is not conserved
  | ValueNotConserved Coin Coin
  -- | The transaction does not have the required witnesses.
  | InsufficientWitnesses
  -- | Missing Replay Attack Protection, at least one input must be spent.
  | InputSetEmpty
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

type Allocs = Map.Map HashKey Slot

-- |The state associated with the current stake delegation.
data DelegationState =
    DelegationState
    {
    -- |The active accounts.
      getAccounts    :: Map.Map RewardAcnt Coin
    -- |The active stake keys.
    , getStKeys      :: Allocs
    -- |The current delegations.
    , getDelegations :: Map.Map HashKey HashKey
    -- |The active stake pools.
    , getStPools     :: Allocs
    -- |Stake pool parameters.
    , getPParams     :: Map.Map HashKey StakePool
    -- |A map of retiring stake pools to the epoch when they retire.
    , getRetiring    :: Map.Map HashKey Epoch
    } deriving (Show, Eq)

emptyDelegation :: DelegationState
emptyDelegation =
    DelegationState Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- |The state associated with a 'Ledger'.
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    getUtxo            :: !UTxO
    -- |The current delegation state
  , getDelegationState :: !DelegationState
    -- |The current protocol constants.
  , getPCs             :: !PrtlConsts
  } deriving (Show, Eq)

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [] Set.empty (Coin 0) (Slot 0))

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState :: PrtlConsts -> [TxOut] -> LedgerState
genesisState pc outs = LedgerState
  (UTxO (Map.fromList
    [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs]
  ))
  emptyDelegation
  pc

-- | Determine if the transaction has expired
current :: TxWits -> Slot -> Validity
current (TxWits tx _) slot =
    if ttl tx < slot
    then Invalid [Expired (ttl tx) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: TxWits -> Validity
validNoReplay (TxWits tx _) =
    if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs :: TxWits -> LedgerState -> Validity
validInputs (TxWits tx _) l =
  if txins tx `Set.isSubsetOf` unspentInputs (getUtxo l)
    then Valid
    else Invalid [BadInputs]
  where unspentInputs (UTxO utxo) = Map.keysSet utxo

-- |Minimum fee calculation
minfee :: PrtlConsts -> Tx -> Coin
minfee pc tx = minfeeA pc * x + minfeeB pc
  where x = Coin $ toEnum $ length (show tx)

-- |Determine if the fee is large enough
validFee :: TxWits -> LedgerState -> Validity
validFee (TxWits tx _) l =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee (getPCs l) tx
        given = fee tx

-- |Compute the lovelace which are consumed by the transaction
destroyed :: Tx -> LedgerState -> Coin
destroyed tx l = balance (txouts tx) + fee tx + deposits (getPCs l) stpools tx
  where stpools = getStPools $ getDelegationState l

-- |Compute the key deregistration refunds in a transaction
keyRefunds :: PrtlConsts -> Allocs -> Tx -> Coin
keyRefunds pc stkeys tx =
  sum [refund' key | (RegKey key) <- Set.toList (certs tx)]
  where refund' key =
          case Map.lookup (hashKey key) stkeys of
            Nothing -> Coin 0
            Just s -> refund (RegKey key) pc (ttl tx -* s)

-- |Compute the lovelace which are created by the transaction
created :: Tx -> LedgerState -> Coin
created tx l = balance (txins tx <| getUtxo l) + refunds
  where refunds = keyRefunds (getPCs l) (getStKeys $ getDelegationState l) tx

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance :: TxWits -> LedgerState -> Validity
preserveBalance (TxWits tx _) l =
  if created' == destroyed'
    then Valid
    else Invalid [ValueNotConserved created' destroyed']
  where
    created' = created tx l
    destroyed' = destroyed tx l

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
    else Invalid [InsufficientWitnesses]
  where
    utxo = getUtxo l
    ins = inputs tx
    hasWitness witnesses input =
      isJust $ find (isWitness tx input utxo) witnesses
    isWitness tx' input unspent (Wit key sig) =
      verify key tx' sig && authTxin key input unspent

validTx :: TxWits -> Slot -> LedgerState -> Validity
validTx tx slot l =
  validInputs tx l
    <> current tx slot
    <> validNoReplay tx
    <> validFee tx l
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
    Delegate (Delegation source _)
      -> if Map.member (hashKey source) (getStKeys ds)
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

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition
  :: Slot -> LedgerState -> TxWits -> Either [ValidationError] LedgerState
asStateTransition slot ls tx =
  case validTx tx slot ls of
    Invalid errors -> Left errors
    Valid          -> foldM (certAsStateTransition slot) ls' cs
      where
        ls' = applyTxBody ls (body tx)
        cs = certs . body $ tx

-- |In the case where a certificate is valid for a given ledger state,
-- apply the certificate as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
certAsStateTransition
  :: Slot -> LedgerState -> DCert -> Either [ValidationError] LedgerState
certAsStateTransition slot ls cert =
  case validDelegation cert ls of
    Invalid errors -> Left errors
    Valid          -> Right $ applyDCert slot cert ls

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: Slot -> LedgerValidation -> TxWits -> LedgerValidation
asStateTransition' slot (LedgerValidation valErrors ls) tx =
    let ls' = applyTxBody ls (body tx) in
    case validTx tx slot ls of
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

applyDCert slot (RegPool sp) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getStPools = Map.insert hsk slot' pools
    , getPParams = Map.insert hsk sp pparams
    , getRetiring = Map.delete hsk (getRetiring ds)}}
  where hsk = hashKey $ poolPubKey sp
        pools = getStPools ds
        pparams = getPParams ds
        slot' = fromMaybe slot (Map.lookup hsk pools)

-- TODO check epoch (not in new doc atm.)
applyDCert _ (RetirePool key epoch) ls@(LedgerState _ ds _) = ls
  { getDelegationState = ds
    { getRetiring = retiring}}
  where retiring = Map.insert hk_sp epoch (getRetiring ds)
        hk_sp = hashKey key

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
