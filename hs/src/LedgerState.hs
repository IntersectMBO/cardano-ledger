{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

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
  , UTxOState(..)
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
import           Data.List               (find, foldl')
import qualified Data.Map                as Map
import           Data.Maybe              (isJust, mapMaybe, fromMaybe)
import           Numeric.Natural         (Natural)
import qualified Data.Set                as Set

import           Coin                    (Coin (..))
import           Slot                    (Slot (..), Epoch (..), (-*), slotFromEpoch)
import           Keys
import           UTxO
import           PrtlConsts              (PrtlConsts(..))

import           Delegation.Certificates (DCert (..), refund)
import           Delegation.StakePool    (Delegation (..), StakePool (..))

import Control.State.Transition

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
  -- | Pool Retirement Certificate expired
  | RetirementCertExpired Slot Slot
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
      _accounts    :: Map.Map RewardAcnt Coin
    -- |The active stake keys.
    , _stKeys      :: Allocs
    -- |The current delegations.
    , _delegations :: Map.Map HashKey HashKey
    -- |The active stake pools.
    , _stPools     :: Allocs
    -- |Stake pool parameters.
    , _pParams     :: Map.Map HashKey StakePool
    -- |A map of retiring stake pools to the epoch when they retire.
    , _retiring    :: Map.Map HashKey Epoch
    } deriving (Show, Eq)

emptyDelegation :: DelegationState
emptyDelegation =
    DelegationState Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

data UTxOState =
    UTxOState
    {
      _utxo      :: !UTxO
    , _deposits  :: Coin
    , _fees      :: Coin
    } deriving (Show, Eq)

-- |The state associated with a 'Ledger'.
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    _utxoState         :: !UTxOState
    -- |The current delegation state
  , _delegationState   :: !DelegationState
    -- |The current protocol constants.
  , _pcs               :: !PrtlConsts
  } deriving (Show, Eq)

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [] [] (Coin 0) (Slot 0))

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState :: PrtlConsts -> [TxOut] -> LedgerState
genesisState pc outs = LedgerState
  (UTxOState
    (UTxO $ Map.fromList
              [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs])
    (Coin 0)
    (Coin 0))
  emptyDelegation
  pc

-- | Determine if the transaction has expired
current :: TxWits -> Slot -> Validity
current (TxWits tx _) slot =
    if _ttl tx < slot
    then Invalid [Expired (_ttl tx) slot]
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
  if txins tx `Set.isSubsetOf` dom ((_utxo . _utxoState) l)
    then Valid
    else Invalid [BadInputs]

-- |Implementation of abstract transaction size
txsize :: Tx -> Natural
txsize = toEnum . length . show

-- |Minimum fee calculation
minfee :: PrtlConsts -> Tx -> Coin
minfee pc tx = Coin $ _minfeeA pc * (txsize tx) + _minfeeB pc

-- |Determine if the fee is large enough
validFee :: TxWits -> LedgerState -> Validity
validFee (TxWits tx _) l =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee (_pcs l) tx
        given = _txfee tx

-- |Compute the lovelace which are consumed by the transaction
destroyed :: Tx -> LedgerState -> Coin
destroyed tx l = balance (txouts tx) + _txfee tx + deposits (_pcs l) stpools tx
  where stpools = _stPools $ _delegationState l

-- |Compute the key deregistration refunds in a transaction
keyRefunds :: PrtlConsts -> Allocs -> Tx -> Coin
keyRefunds pc stkeys tx =
  sum [refund' key | (RegKey key) <- _certs tx]
  where refund' key =
          case Map.lookup (hashKey key) stkeys of
            Nothing -> Coin 0
            Just s -> refund (RegKey key) pc (_ttl tx -* s)

-- |Compute the lovelace which are created by the transaction
created :: Tx -> LedgerState -> Coin
created tx l = balance (txins tx <| (_utxo . _utxoState) l) + refunds
  where refunds = keyRefunds (_pcs l) (_stKeys $ _delegationState l) tx

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
    utxo = (_utxo . _utxoState) l
    ins  = _inputs tx
    hasWitness witnesses input =
      isJust $ find (isWitness tx input utxo) witnesses
    isWitness tx' input unspent (Wit key sig) =
      verify key tx' sig && authTxin key input unspent

validRuleUTXO :: TxWits -> Slot -> LedgerState -> Validity
validRuleUTXO tx slot l = validInputs tx l
                       <> current tx slot
                       <> validNoReplay tx
                       <> validFee tx l
                       <> preserveBalance tx l
                       <> validCertsRetirePoolNotExpired tx slot

validRuleUTXOW :: TxWits -> Slot -> LedgerState -> Validity
validRuleUTXOW tx _ l = witnessed tx l

validTx :: TxWits -> Slot -> LedgerState -> Validity
validTx tx slot l = validRuleUTXO  tx slot l
                 <> validRuleUTXOW tx slot l

-- The rules for checking validiy of stake delegation transitions return
-- `certificate_type_correct(cert) -> valid_cert(cert)`, i.e., if the
-- certificate is of a different type, it's considered to be valid due to the
-- falsified hypothesis.

-- | Checks whether a key registration certificat is valid.
validKeyRegistration :: DCert -> LedgerState -> Validity
validKeyRegistration cert (LedgerState _ ds _) =
  case cert of
    RegKey key -> if not $ Map.member (hashKey key) (_stKeys ds)
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
    _          -> Valid

validKeyDeregistration :: DCert -> LedgerState -> Validity
validKeyDeregistration cert (LedgerState _ ds _) =
  case cert of
    DeRegKey key -> if Map.member (hashKey key) (_stKeys ds)
                    then Valid else Invalid [StakeKeyNotRegistered]
    _            -> Valid

validStakeDelegation :: DCert -> LedgerState -> Validity
validStakeDelegation cert (LedgerState _ ds _) =
  case cert of
    Delegate (Delegation source _)
      -> if Map.member (hashKey source) (_stKeys ds)
         then Valid else Invalid [StakeDelegationImpossible]
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister :: DCert -> LedgerState -> Validity
validStakePoolRegister _ _ = Valid

-- | Validate that if the certificate is a pool retirement certificate, then we
-- have not yet reached or passed its latest possible application slot.
validCertRetirePoolNotExpired :: Slot -> DCert -> Validity
validCertRetirePoolNotExpired ttlSlot cert  =
    case cert of
      (RetirePool _ epoch) ->
          if ttlSlot <= slotFromEpoch epoch
          then Valid
          else Invalid [RetirementCertExpired ttlSlot (slotFromEpoch epoch)]
      _                    -> Valid

validCertsRetirePoolNotExpired :: TxWits -> Slot -> Validity
validCertsRetirePoolNotExpired tx slot =
    foldl' (\validity cert -> validity <> validCertRetirePoolNotExpired slot cert) Valid $ (_certs . _body $ tx)

validStakePoolRetire :: DCert -> LedgerState -> Validity
validStakePoolRetire cert (LedgerState _ ds _) =
  case cert of
    RetirePool key _ -> if Map.member (hashKey key) (_stPools ds)
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
        ls' = applyTxBody ls (_body tx)
        cs = _certs . _body $ tx

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
    let ls' = applyTxBody ls (_body tx) in
    case validTx tx slot ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Epoch -> LedgerState
retirePools ls@(LedgerState _ ds _) epoch = ls
    { _delegationState = ds
      { _stPools =
          Map.filterWithKey
           (\hk _ -> Map.notMember hk retiring)
           (_stPools ds)
      , _retiring = active }
    }
  where (active, retiring) = Map.partition (epoch /=) (_retiring ds)

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: LedgerState -> Tx -> LedgerState
applyTxBody ls tx = ls { _utxoState = oldUTxOstate { _utxo = newUTxOs } }
  where newUTxOs = txins tx </| (_utxo . _utxoState) ls `union` txouts tx
        oldUTxOstate = _utxoState ls

-- |Apply a delegation certificate as a state transition function on the ledger state.
applyDCert :: Slot -> DCert -> LedgerState -> LedgerState
applyDCert slot (RegKey key) ls@(LedgerState _ ds _) = ls
  { _delegationState = ds
    { _stKeys = Map.insert hksk slot (_stKeys ds)
    , _accounts = Map.insert (RewardAcnt hksk) (Coin 0) (_accounts ds)}
  }
  where hksk = hashKey key

applyDCert _ (DeRegKey key) ls@(LedgerState _ ds _) = ls
  { _delegationState = ds
    { _stKeys = Map.delete hksk (_stKeys ds)
    , _accounts = Map.delete (RewardAcnt hksk) (_accounts ds)
    , _delegations = Map.delete hksk (_delegations ds) }
  }
  where hksk = hashKey key

-- TODO do we also have to check hashKey target?
applyDCert _ (Delegate (Delegation source target)) ls@(LedgerState _ ds _) = ls
  {_delegationState = ds
    { _delegations =
        Map.insert (hashKey source) (hashKey target) (_delegations ds)}
  }

applyDCert slot (RegPool sp) ls@(LedgerState _ ds _) = ls
  { _delegationState = ds
    { _stPools = Map.insert hsk slot' pools
    , _pParams = Map.insert hsk sp pparams
    , _retiring = Map.delete hsk (_retiring ds)}}
  where hsk = hashKey $ _poolPubKey sp
        pools = _stPools ds
        pparams = _pParams ds
        slot' = fromMaybe slot (Map.lookup hsk pools)

-- TODO check epoch (not in new doc atm.)
applyDCert _ (RetirePool key epoch) ls@(LedgerState _ ds _) = ls
  { _delegationState = ds
    { _retiring = retiring}}
  where retiring = Map.insert hk_sp epoch (_retiring ds)
        hk_sp = hashKey key

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map.Map HashKey Coin
delegatedStake ls@(LedgerState _ ds _) = Map.fromListWith mappend delegatedOutputs
  where
    getOutputs (UTxO utxo) = Map.elems utxo
    addStake delegations (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup hsk delegations
      return (pool, c)
    outs = getOutputs . (_utxo . _utxoState) $ ls
    delegatedOutputs = mapMaybe (addStake (_delegations ds)) outs

---------------------------------------------------------------------------------
-- State transition system
---------------------------------------------------------------------------------

data UTXO

instance STS UTXO where
    type State UTXO = LedgerState
    type Signal UTXO = TxWits
    type Environment UTXO = Slot
    data PredicateFailure UTXO = UTXOFailure [ValidationError]
                   deriving (Eq, Show)

    rules = [applyTxWits]

fromValidity :: Validity -> PredicateResult UTXO
fromValidity Valid = Passed
fromValidity (Invalid xs) = Failed $ UTXOFailure xs

executeTransition :: LedgerState -> TxWits -> LedgerState
executeTransition lstate txwit = applyTxBody lstate (_body txwit)

applyTxWits :: Rule UTXO
applyTxWits =
    Rule
    [
     Predicate $ \(slot, lstate, txwit) -> fromValidity $ validRuleUTXO txwit slot lstate
    ]
    ( Extension . Transition $ \(_, lstate, txwit) ->
          executeTransition lstate txwit
    )
