{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('Tx')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}

module LedgerState
  ( LedgerState(..)
  , Ix
  , DPState(..)
  , DState(..)
  , AccountState(..)
  , RewardUpdate(..)
  , emptyRewardUpdate
  , EpochState(..)
  , emptyEpochState
  , emptyLedgerState
  , dstate
  , pstate
  , ptrs
  , fdms
  , dms
  , PState(..)
  , cCounters
  , LedgerValidation(..)
  , KeyPairs
  , UTxOState(..)
  , StakeShare(..)
  , Validity(..)
  , mkStakeShare
  , emptyAccount
  , emptyPState
  , emptyDState
  , poolRAcnt
  , treasury
  , reserves
  -- * state transitions
  , asStateTransition
  , asStateTransition'
  , delegatedStake
  , retirePools
  , emptyDelegation
  , applyDCert
  , applyDCertDState
  , applyDCertPState
  , applyUTxOUpdate
  -- * Genesis State
  , genesisId
  , genesisCoins
  , genesisState
  -- * Validation
  , ValidationError (..)
  , minfee
  , validStakePoolRetire
  , validInputs
  , validNoReplay
  , validFee
  , validKeyRegistration
  , validKeyDeregistration
  , validStakeDelegation
  , preserveBalance
  , verifiedWits
  , witsVKeyNeeded
  -- lenses
  , utxoState
  , delegationState
  -- UTxOState
  , utxo
  , deposited
  , fees
  , ups
  -- DelegationState
  , rewards
  , stKeys
  , delegations
  , stPools
  , pParams
  , retiring
  -- refunds
  , keyRefunds
  , keyRefund
  , decayedKey
  , decayedTx
  , poolRefunds
  -- epoch boundary
  , poolRewards
  , leaderRew
  , memberRew
  , rewardOnePool
  , reward
  , stakeDistr
  , poolDistr
  , applyRUpd
  , createRUpd
  --
  , NewEpochState(..)
  , NewEpochEnv(..)
  , overlaySchedule
  , getGKeys
  , setIssueNumbers
  , updateNES
  ) where

import           Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Numeric.Natural (Natural)

import           Lens.Micro ((%~), (&), (.~), (^.))
import           Lens.Micro.TH (makeLenses)

import           Coin (Coin (..))
import           EpochBoundary (BlocksMade (..), SnapShots (..), Stake (..), baseStake, consolidate,
                     emptySnapShots, maxPool, poolRefunds, poolStake, ptrStake, rewardStake)
import           Keys (DSIGNAlgorithm, Dms (..), HashAlgorithm, KeyHash, KeyPair, Signable, VKey,
                     VKeyGenesis, hash, hashKey)
import           PParams (PParams (..), emptyPParams, keyDecayRate, keyDeposit, keyMinRefund,
                     minfeeA, minfeeB)
import           Slot (Epoch (..), Slot (..), epochFromSlot, firstSlot, slotsPerEpoch, (-*))
import           Tx (extractKeyHash)
import           TxData (Addr (..), Credential (..), Delegation (..), Ix, PoolParams, Ptr (..),
                     RewardAcnt (..), StakeCredential, Tx (..), TxBody (..), TxId (..), TxIn (..),
                     TxOut (..), WitVKey (..), body, certs, getRwdHK, inputs, poolOwners,
                     poolPledge, poolPubKey, poolRAcnt, ttl, txfee, wdrls)
import           Updates (AVUpdate (..), Applications, PPUpdate (..), Update (..), emptyUpdate,
                     emptyUpdateState)
import           UTxO (UTxO (..), balance, deposits, dom, txinLookup, txins, txouts, txup, union,
                     verifyWitVKey, (</|), (<|))

import           Delegation.Certificates (DCert (..), PoolDistr (..), StakeKeys (..),
                     StakePools (..), cwitness, decayKey, refund)
import           Delegation.PoolParams (poolSpec)

import           BaseTypes (Seed (..), UnitInterval, intervalValue, mkUnitInterval)

import           Ledger.Core ((∪+), (▷), (◁))

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs dsignAlgo = [(KeyPair dsignAlgo, KeyPair dsignAlgo)]

-- | A ledger validation state consists of a ledger state 't' and the list of
-- validation errors that occurred from a valid 's' to reach 't'.
data LedgerValidation hashAlgo dsignAlgo
  = LedgerValidation [ValidationError] (LedgerState hashAlgo dsignAlgo)
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
  -- | Unknown reward account
  | IncorrectRewards
  -- | One of the transaction witnesses is invalid.
  | InvalidWitness
  -- | The transaction does not have the required witnesses.
  | MissingWitnesses
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

type RewardAccounts hashAlgo dsignAlgo
  = Map.Map (RewardAcnt hashAlgo dsignAlgo) Coin

-- | StakeShare type
newtype StakeShare =
  StakeShare Rational
  deriving (Show, Ord, Eq)

-- | Construct an optional probability value
mkStakeShare :: Rational -> Maybe StakeShare
mkStakeShare p =
  if 0 <= p
    then Just $ StakeShare p
    else Nothing

data DState hashAlgo dsignAlgo = DState
    {  -- |The active stake keys.
      _stKeys      :: StakeKeys hashAlgo dsignAlgo
      -- |The active accounts.
    ,  _rewards    :: RewardAccounts hashAlgo dsignAlgo
      -- |The current delegations.
    , _delegations :: Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
      -- |The pointed to hash keys.
    , _ptrs        :: Map.Map Ptr (StakeCredential hashAlgo dsignAlgo)
      -- | future genesis key delegations
    , _fdms        :: Map.Map (Slot, VKeyGenesis dsignAlgo) (VKey dsignAlgo)
      -- |Genesis key delegations
    , _dms         :: Dms dsignAlgo
    } deriving (Show, Eq)

data PState hashAlgo dsignAlgo = PState
    { -- |The active stake pools.
      _stPools     :: StakePools hashAlgo dsignAlgo
      -- |The pool parameters.
    , _pParams     :: Map.Map (KeyHash hashAlgo dsignAlgo) (PoolParams hashAlgo dsignAlgo)
      -- |A map of retiring stake pools to the epoch when they retire.
    , _retiring    :: Map.Map (KeyHash hashAlgo dsignAlgo) Epoch
      -- | Operational Certificate Counters.
    , _cCounters   :: Map.Map (KeyHash hashAlgo dsignAlgo) Natural
    } deriving (Show, Eq)

-- |The state associated with the current stake delegation.
data DPState hashAlgo dsignAlgo =
    DPState
    {
      _dstate :: DState hashAlgo dsignAlgo
    , _pstate :: PState hashAlgo dsignAlgo
    } deriving (Show, Eq)

data RewardUpdate hashAlgo dsignAlgo = RewardUpdate
  { deltaT :: Coin
  , deltaR :: Coin
  , rs     :: Map.Map (RewardAcnt hashAlgo dsignAlgo) Coin
  , deltaF :: Coin
  } deriving (Show, Eq)

emptyRewardUpdate :: RewardUpdate hashAlgo dsignAlgo
emptyRewardUpdate = RewardUpdate (Coin 0) (Coin 0) Map.empty (Coin 0)

data AccountState = AccountState
  { _treasury  :: Coin
  , _reserves  :: Coin
  } deriving (Show, Eq)

data EpochState hashAlgo dsignAlgo
  = EpochState
      AccountState
      (SnapShots hashAlgo dsignAlgo)
      (LedgerState hashAlgo dsignAlgo)
      PParams
  deriving (Show, Eq)

emptyEpochState :: EpochState hashAlgo dsignAlgo
emptyEpochState =
  EpochState emptyAccount emptySnapShots emptyLedgerState  emptyPParams

emptyLedgerState :: LedgerState hashAlgo dsignAlgo
emptyLedgerState =
  LedgerState
  (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)
  emptyDelegation
  0

emptyAccount :: AccountState
emptyAccount = AccountState (Coin 0) (Coin 0)

emptyDelegation :: DPState hashAlgo dsignAlgo
emptyDelegation =
    DPState emptyDState emptyPState

emptyDState :: DState hashAlgo dsignAlgo
emptyDState =
  DState (StakeKeys Map.empty) Map.empty Map.empty Map.empty Map.empty (Dms Map.empty)

emptyPState :: PState hashAlgo dsignAlgo
emptyPState =
  PState (StakePools Map.empty) Map.empty Map.empty Map.empty

data UTxOState hashAlgo dsignAlgo =
    UTxOState
    { _utxo      :: !(UTxO hashAlgo dsignAlgo)
    , _deposited :: Coin
    , _fees      :: Coin
    , _ups       :: ( PPUpdate dsignAlgo
                    , AVUpdate dsignAlgo
                    , Map.Map Slot Applications
                    , Applications)
    } deriving (Show, Eq)

-- | New Epoch state and environment
data NewEpochState hashAlgo dsignAlgo =
  NewEpochState {
    nesEL    :: Epoch
  , nesEta0  :: Seed
  , nesBprev :: BlocksMade hashAlgo dsignAlgo
  , nesBcur  :: BlocksMade hashAlgo dsignAlgo
  , nesEs    :: EpochState hashAlgo dsignAlgo
  , nesRu    :: Maybe (RewardUpdate hashAlgo dsignAlgo)
  , nesPd    :: PoolDistr hashAlgo dsignAlgo
  , nesOsched :: Map.Map Slot (Maybe (VKeyGenesis dsignAlgo))
  } deriving (Show, Eq)

getGKeys :: NewEpochState hashAlgo dsignAlgo -> Set (VKeyGenesis dsignAlgo)
getGKeys nes = Map.keysSet dms
  where NewEpochState _ _ _ _ es _ _ _ = nes
        EpochState _ _ ls _ = es
        LedgerState _ (DPState (DState _ _ _ _ _ (Dms dms)) _) _ = ls

data NewEpochEnv dsignAlgo =
  NewEpochEnv {
    neeEta1  :: Seed
  , neeS     :: Slot
  , neeGkeys :: Set.Set (VKeyGenesis dsignAlgo)
  } deriving (Show, Eq)

-- |The state associated with a 'Ledger'.
data LedgerState hashAlgo dsignAlgo =
  LedgerState
  { -- |The current unspent transaction outputs.
    _utxoState         :: !(UTxOState hashAlgo dsignAlgo)
    -- |The current delegation state
  , _delegationState   :: !(DPState hashAlgo dsignAlgo)
    -- |The current transaction index in the current slot.
  , _txSlotIx          :: Ix
  } deriving (Show, Eq)

makeLenses ''DPState
makeLenses ''DState
makeLenses ''PState
makeLenses ''UTxOState
makeLenses ''AccountState
makeLenses ''LedgerState

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => TxId hashAlgo dsignAlgo
genesisId =
  TxId $ hash
  (TxBody
   Set.empty
   []
   []
   Map.empty
   (Coin 0)
   (Slot 0)
   emptyUpdate)

-- |Creates the UTxO for a new ledger with the specified transaction outputs.
genesisCoins
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => [TxOut hashAlgo dsignAlgo]
  -> UTxO hashAlgo dsignAlgo
genesisCoins outs = UTxO $
  Map.fromList [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs]

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => [TxOut hashAlgo dsignAlgo]
  -> LedgerState hashAlgo dsignAlgo
genesisState outs = LedgerState
  (UTxOState
    (genesisCoins outs)
    (Coin 0)
    (Coin 0)
    emptyUpdateState)
  emptyDelegation
  0

-- | Determine if the transaction has expired
current :: TxBody hashAlgo dsignAlgo -> Slot -> Validity
current tx slot =
    if tx ^. ttl < slot
    then Invalid [Expired (tx ^. ttl) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: TxBody hashAlgo dsignAlgo -> Validity
validNoReplay tx =
    if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs
  :: TxBody hashAlgo dsignAlgo
  -> UTxOState hashAlgo dsignAlgo
  -> Validity
validInputs tx u =
  if txins tx `Set.isSubsetOf` dom (u ^. utxo)
    then Valid
    else Invalid [BadInputs]

-- |Implementation of abstract transaction size
txsize
  :: DSIGNAlgorithm dsignAlgo
  => TxBody hashAlgo dsignAlgo
  -> Integer
txsize = toEnum . length . show

-- |Minimum fee calculation
minfee
  :: DSIGNAlgorithm dsignAlgo
  => PParams
  -> TxBody hashAlgo dsignAlgo
  -> Coin
minfee pc tx = Coin $ pc ^. minfeeA * txsize tx + fromIntegral (pc ^. minfeeB)

-- |Determine if the fee is large enough
validFee
  :: DSIGNAlgorithm dsignAlgo
  => PParams
  -> TxBody hashAlgo dsignAlgo
  -> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee pc tx
        given  = tx ^. txfee

-- |Compute the lovelace which are created by the transaction
produced
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => PParams
  -> StakePools hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo
  -> Coin
produced pp stakePools tx =
    balance (txouts tx) + tx ^. txfee + deposits pp stakePools (tx ^. certs)

-- |Compute the key deregistration refunds in a transaction
keyRefunds
  :: PParams
  -> StakeKeys hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo
  -> Coin
keyRefunds pp stk tx =
  sum [keyRefund dval dmin lambda stk (tx ^. ttl) c | c@(DeRegKey _) <- tx ^. certs]
  where (dval, dmin, lambda) = decayKey pp

-- | Key refund for a deregistration certificate.
keyRefund
  :: Coin
  -> UnitInterval
  -> Rational
  -> StakeKeys hashAlgo dsignAlgo
  -> Slot
  -> DCert hashAlgo dsignAlgo
  -> Coin
keyRefund dval dmin lambda (StakeKeys stkeys) slot c =
    case c of
      DeRegKey key -> case Map.lookup key stkeys of
                        Nothing -> Coin 0
                        Just  s -> refund dval dmin lambda $ slot -* s
      _ -> Coin 0

-- | Functions to calculate decayed deposits
decayedKey
  :: PParams
  -> StakeKeys hashAlgo dsignAlgo
  -> Slot
  -> DCert hashAlgo dsignAlgo
  -> Coin
decayedKey pp stk@(StakeKeys stkeys) cslot cert =
    case cert of
      DeRegKey key ->
          if Map.notMember key stkeys
          then 0
          else let created'      = stkeys Map.! key in
               let start         = max (firstSlot $ epochFromSlot cslot) created' in
               let dval          = pp ^. keyDeposit in
               let dmin          = pp ^. keyMinRefund in
               let lambda        = pp ^. keyDecayRate in
               let epochRefund   = keyRefund dval dmin lambda stk start cert in
               let currentRefund = keyRefund dval dmin lambda stk cslot cert in
               epochRefund - currentRefund
      _ -> 0

-- | Decayed deposit portions
decayedTx
  :: PParams
  -> StakeKeys hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo
  -> Coin
decayedTx pp stk tx =
    sum [decayedKey pp stk (tx ^. ttl) c | c@(DeRegKey _) <- tx ^. certs]

-- |Compute the lovelace which are destroyed by the transaction
consumed
  :: PParams
  -> UTxO hashAlgo dsignAlgo
  -> StakeKeys hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo
  -> Coin
consumed pp u stakeKeys tx =
    balance (txins tx <| u) + refunds + withdrawals
  where
    refunds = keyRefunds pp stakeKeys tx
    withdrawals = sum $ tx ^. wdrls

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => StakePools hashAlgo dsignAlgo
  -> StakeKeys hashAlgo dsignAlgo
  -> PParams
  -> TxBody hashAlgo dsignAlgo
  -> UTxOState hashAlgo dsignAlgo
  -> Validity
preserveBalance stakePools stakeKeys pp tx u =
  if destroyed' == created'
    then Valid
    else Invalid [ValueNotConserved destroyed' created']
  where
    destroyed' = consumed pp (u ^. utxo) stakeKeys tx
    created' = produced pp stakePools tx

-- |Determine if the reward witdrawals correspond
-- to the rewards in the ledger state
correctWithdrawals
  :: RewardAccounts hashAlgo dsignAlgo
  -> RewardAccounts hashAlgo dsignAlgo
  -> Validity
correctWithdrawals accs withdrawals =
  if withdrawals `Map.isSubmapOf` accs
    then Valid
    else Invalid [IncorrectRewards]

-- |Collect the set of hashes of keys that needs to sign a
-- given transaction. This set consists of the txin owners,
-- certificate authors, and withdrawal reward accounts.
witsVKeyNeeded
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => UTxO hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Dms dsignAlgo
  -> Set (KeyHash hashAlgo dsignAlgo)
witsVKeyNeeded utxo' tx@(Tx txbody _ _) _dms =
    inputAuthors `Set.union`
    wdrlAuthors  `Set.union`
    certAuthors  `Set.union`
    updateKeys   `Set.union`
    owners
  where
    inputAuthors = Set.foldr insertHK Set.empty (txbody ^. inputs)
    insertHK txin hkeys =
      case txinLookup txin utxo' of
        Just (TxOut (AddrBase (KeyHashObj pay) _) _) -> Set.insert pay hkeys
        _                               -> hkeys

    wdrlAuthors =
      Set.fromList $ extractKeyHash $ map getRwdHK (Map.keys (txbody ^. wdrls))
    owners = foldl Set.union Set.empty
               [pool ^. poolOwners | RegPool pool <- txbody ^. certs]
    certAuthors = Set.fromList $ extractKeyHash (fmap getCertHK (txbody ^. certs))
    getCertHK = cwitness
    updateKeys = propWits (txup tx) _dms

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are correct.
verifiedWits
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => Tx hashAlgo dsignAlgo
  -> Validity
verifiedWits (Tx tx wits _) =
  if all (verifyWitVKey tx) wits
    then Valid
    else Invalid [InvalidWitness]

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- We check that there are not more witnesses than inputs, if several inputs
-- from the same address are used, it is not strictly necessary to include more
-- than one witness.
enoughWits
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Tx hashAlgo dsignAlgo
  -> Dms dsignAlgo
  -> UTxOState hashAlgo dsignAlgo
  -> Validity
enoughWits tx@(Tx _ wits _) d u =
  if witsVKeyNeeded (u ^. utxo) tx d `Set.isSubsetOf` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = Set.map (\(WitVKey vkey _) -> hashKey vkey) wits

validRuleUTXO
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => RewardAccounts hashAlgo dsignAlgo
  -> StakePools hashAlgo dsignAlgo
  -> StakeKeys hashAlgo dsignAlgo
  -> PParams
  -> Slot
  -> TxBody hashAlgo dsignAlgo
  -> UTxOState hashAlgo dsignAlgo
  -> Validity
validRuleUTXO accs stakePools stakeKeys pc slot tx u =
                          validInputs tx u
                       <> current tx slot
                       <> validNoReplay tx
                       <> validFee pc tx
                       <> preserveBalance stakePools stakeKeys pc tx u
                       <> correctWithdrawals accs (tx ^. wdrls)

validRuleUTXOW
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => Tx hashAlgo dsignAlgo
  -> Dms dsignAlgo
  -> LedgerState hashAlgo dsignAlgo
  -> Validity
validRuleUTXOW tx d l = verifiedWits tx
                   <> enoughWits tx d (l ^. utxoState)

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Update dsignAlgo
  -> Dms dsignAlgo
  -> Set.Set (KeyHash hashAlgo dsignAlgo)
propWits (Update (PPUpdate pup) (AVUpdate aup')) (Dms _dms) =
  Set.fromList $ Map.elems $ Map.map hashKey updateKeys
  where updateKeys = (Map.keysSet pup `Set.union` Map.keysSet aup') ◁ _dms

validTx
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => Tx hashAlgo dsignAlgo
  -> Dms dsignAlgo
  -> Slot
  -> PParams
  -> LedgerState hashAlgo dsignAlgo
  -> Validity
validTx tx d slot pp l =
    validRuleUTXO  (l ^. delegationState . dstate . rewards)
                   (l ^. delegationState . pstate . stPools)
                   (l ^. delegationState . dstate . stKeys)
                   pp
                   slot
                   (tx ^. body)
                   (l ^. utxoState)
 <> validRuleUTXOW tx d l

-- The rules for checking validiy of stake delegation transitions return
-- `certificate_type_correct(cert) -> valid_cert(cert)`, i.e., if the
-- certificate is of a different type, it's considered to be valid due to the
-- falsified hypothesis.

-- | Checks whether a key registration certificat is valid.
validKeyRegistration
  :: DCert hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
  -> Validity
validKeyRegistration cert ds =
  case cert of
    RegKey key -> if not $ Map.member key stakeKeys
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
                   where (StakeKeys stakeKeys) = ds ^. stKeys
    _          -> Valid

validKeyDeregistration
  :: DCert hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
  -> Validity
validKeyDeregistration cert ds =
  case cert of
    DeRegKey key -> if Map.member key stakeKeys
                    then Valid else Invalid [StakeKeyNotRegistered]
                      where (StakeKeys stakeKeys) = ds ^. stKeys
    _            -> Valid

validStakeDelegation
  ::  DCert hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
  -> Validity
validStakeDelegation cert ds =
  case cert of
    Delegate (Delegation source _)
      -> if Map.member source stakeKeys
         then Valid else Invalid [StakeDelegationImpossible]
           where (StakeKeys stakeKeys) = ds ^. stKeys
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister
  :: DCert hashAlgo dsignAlgo
  -> DPState hashAlgo dsignAlgo
  -> Validity
validStakePoolRegister _ _ = Valid

validStakePoolRetire
  :: DCert hashAlgo dsignAlgo
  -> PState hashAlgo dsignAlgo
  -> Validity
validStakePoolRetire cert ps =
  case cert of
    RetirePool key _ -> if Map.member key stakePools
                        then Valid else Invalid [StakePoolNotRegisteredOnKey]
                         where (StakePools stakePools) = ps ^. stPools
    _                -> Valid

validDelegation
  :: DCert hashAlgo dsignAlgo
  -> DPState hashAlgo dsignAlgo
  -> Validity
validDelegation cert ds =
     validKeyRegistration cert (ds ^. dstate)
  <> validKeyDeregistration cert (ds ^. dstate)
  <> validStakeDelegation cert (ds ^. dstate)
  <> validStakePoolRegister cert ds
  <> validStakePoolRetire cert (ds ^. pstate)

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => Slot
  -> PParams
  -> LedgerState hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Dms dsignAlgo
  -> Either [ValidationError] (LedgerState hashAlgo dsignAlgo)
asStateTransition slot pp ls tx d =
  case validTx tx d slot pp ls of
    Invalid errors -> Left errors
    Valid          -> foldM (certAsStateTransition slot (ls ^. txSlotIx)) ls' cs
      where
        ls' = applyTxBody ls pp (tx ^. body)
        cs = zip [0..] (tx ^. body . certs) -- index certificates

-- |In the case where a certificate is valid for a given ledger state,
-- apply the certificate as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
certAsStateTransition
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Slot
  -> Ix
  -> LedgerState hashAlgo dsignAlgo
  -> (Ix, DCert hashAlgo dsignAlgo)
  -> Either [ValidationError] (LedgerState hashAlgo dsignAlgo)
certAsStateTransition slot txIx ls (clx, cert) =
  case validDelegation cert (ls ^. delegationState) of
    Invalid errors -> Left errors
    Valid          -> Right $ ls & delegationState %~ applyDCert (Ptr slot txIx clx) cert

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => Slot
  -> PParams
  -> LedgerValidation hashAlgo dsignAlgo
  -> Tx hashAlgo dsignAlgo
  -> Dms dsignAlgo
  -> LedgerValidation hashAlgo dsignAlgo
asStateTransition' slot pp (LedgerValidation valErrors ls) tx d =
    let ls' = applyTxBody ls pp (tx ^. body) in
    case validTx tx d slot pp ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools
  :: LedgerState hashAlgo dsignAlgo
  -> Epoch
  -> LedgerState hashAlgo dsignAlgo
retirePools ls@(LedgerState _ ds _) epoch =
    ls & delegationState .~
           (ds & pstate . stPools .~
                 (StakePools $ Map.filterWithKey
                        (\hk _ -> Map.notMember hk retiring')
                        stakePools)
               & pstate . retiring .~ active)
  where (active, retiring') = Map.partition (epoch /=) (ds ^. pstate . retiring)
        (StakePools stakePools) = ds ^. pstate . stPools

-- |Calculate the change to the deposit pool for a given transaction.
depositPoolChange
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => LedgerState hashAlgo dsignAlgo
  -> PParams
  -> TxBody hashAlgo dsignAlgo
  -> Coin
depositPoolChange ls pp tx = (currentPool + txDeposits) - txRefunds
  -- Note that while (currentPool + txDeposits) >= txRefunds,
  -- it could be that txDeposits < txRefunds. We keep the parenthesis above
  -- to emphasize this point.
  where
    currentPool = ls ^. utxoState . deposited
    txDeposits =
      deposits pp (ls ^. delegationState . pstate . stPools) (tx ^. certs)
    txRefunds = keyRefunds pp (ls ^. delegationState . dstate . stKeys) tx

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => LedgerState hashAlgo dsignAlgo
  -> PParams
  -> TxBody hashAlgo dsignAlgo
  -> LedgerState hashAlgo dsignAlgo
applyTxBody ls pp tx =
    ls & utxoState %~ flip applyUTxOUpdate tx
       & utxoState . deposited .~ depositPoolChange ls pp tx
       & utxoState . fees .~ (tx ^. txfee) + (ls ^. utxoState . fees)
       & delegationState . dstate . rewards .~ newAccounts
       & txSlotIx  %~ (+) 1
  where
    newAccounts = reapRewards (ls ^. delegationState . dstate. rewards) (tx ^. wdrls)

reapRewards
  :: RewardAccounts hashAlgo dsignAlgo
  -> RewardAccounts hashAlgo dsignAlgo
  -> RewardAccounts hashAlgo dsignAlgo
reapRewards dStateRewards withdrawals =
    Map.mapWithKey removeRewards dStateRewards
    where removeRewards k v = if k `Map.member` withdrawals then Coin 0 else v

applyUTxOUpdate
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => UTxOState hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo
  -> UTxOState hashAlgo dsignAlgo
applyUTxOUpdate u tx = u & utxo .~ txins tx </| (u ^. utxo) `union` txouts tx

-- |Apply a delegation certificate as a state transition function on the ledger state.
applyDCert
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Ptr
  -> DCert hashAlgo dsignAlgo
  -> DPState hashAlgo dsignAlgo
  -> DPState hashAlgo dsignAlgo

applyDCert ptr dcert@(RegKey _) ds =
  ds & dstate %~ applyDCertDState ptr dcert

applyDCert ptr dcert@(DeRegKey _) ds =
  ds & dstate %~ applyDCertDState ptr dcert

applyDCert ptr dcert@(RegPool _) ds = ds & pstate %~ applyDCertPState ptr dcert

applyDCert ptr dcert@(RetirePool _ _) ds =
  ds & pstate %~ applyDCertPState ptr dcert

applyDCert _ (GenesisDelegate _) ds = ds -- TODO: check this

-- TODO do we also have to check hashKey target?
applyDCert ptr dcert@(Delegate _) ds =
  ds & dstate %~ applyDCertDState ptr dcert

applyDCertDState
  :: Ptr
  -> DCert hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
applyDCertDState (Ptr slot txIx clx) (DeRegKey key) ds =
    ds & stKeys      .~ (StakeKeys $ Map.delete hksk stkeys')
       & rewards     %~ Map.delete (RewardAcnt hksk)
       & delegations %~ Map.delete hksk
       & ptrs        %~ Map.delete (Ptr slot txIx clx)
        where hksk = key
              (StakeKeys stkeys') = ds ^. stKeys

applyDCertDState (Ptr slot txIx clx) (RegKey key) ds =
    ds & stKeys  .~ (StakeKeys $ Map.insert hksk slot stkeys')
       & rewards %~ Map.insert (RewardAcnt hksk) (Coin 0)
       & ptrs    %~ Map.insert (Ptr slot txIx clx) hksk
        where hksk = key
              (StakeKeys stkeys') = ds ^. stKeys

applyDCertDState _ (Delegate (Delegation source target)) ds =
    ds & delegations %~ Map.insert source target

applyDCertDState _ _ ds = ds

applyDCertPState
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo)
  => Ptr
  -> DCert hashAlgo dsignAlgo
  -> PState hashAlgo dsignAlgo
  -> PState hashAlgo dsignAlgo
applyDCertPState (Ptr slot _ _ ) (RegPool sp) ps =
    ps & stPools  .~ (StakePools $ Map.insert hsk slot' pools)
       & pParams  %~ Map.insert hsk sp
       & retiring %~ Map.delete hsk
  where hsk = hashKey $ sp ^. poolPubKey
        (StakePools pools) = ps ^. stPools
        slot' = fromMaybe slot (Map.lookup hsk pools)

-- TODO check epoch (not in new doc atm.)
applyDCertPState _ (RetirePool key epoch) ps =
  ps & retiring %~ Map.insert key epoch

-- | Use onlt pool registration or retirement certificates
applyDCertPState _ _ ps = ps

-- |Compute how much stake each active stake pool controls.
delegatedStake
  :: LedgerState hashAlgo dsignAlgo
  -> Map.Map (KeyHash hashAlgo dsignAlgo) Coin
delegatedStake ls@(LedgerState _ ds _) = Map.fromListWith (+) delegatedOutputs
  where
    getOutputs (UTxO utxo') = Map.elems utxo'
    addStake delegs (TxOut (AddrBase _ (KeyHashObj hsk)) c) = do
      pool <- Map.lookup (KeyHashObj hsk) delegs
      return (pool, c)
    addStake _ (TxOut (AddrBase _ _) _) = undefined -- TODO: script addresses
    addStake _ (TxOut (AddrEnterprise _) _) = undefined -- TODO: script addresses
    addStake delegs (TxOut (AddrPtr ptr) c) = do
      key  <- Map.lookup ptr $ ds ^. dstate . ptrs
      pool <- Map.lookup key delegs
      return (pool, c)
    outs = getOutputs $ ls ^. utxoState . utxo
    delegatedOutputs = mapMaybe (addStake $ ds ^. dstate . delegations) outs

---------------------------------
-- epoch boundary calculations --
---------------------------------

-- | Calculate pool reward
poolRewards
  :: StakeCredential hashAlgo dsignAlgo -- TODO check why this paramater is not used
  -> UnitInterval
  -> Natural
  -> Natural
  -> Coin
  -> Coin
poolRewards _ sigma blocksN blocksTotal (Coin maxP) =
  floor $ p * fromIntegral maxP
  where
    p = beta / intervalValue sigma
    beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)

-- | Calculate pool leader reward
leaderRew
  :: Coin
  -> PoolParams hashAlgo dsignAlgo
  -> StakeShare
  -> StakeShare
  -> Coin
leaderRew f@(Coin f') pool (StakeShare s) (StakeShare sigma)
  | f' <= c = f
  | otherwise =
    floor $ fromIntegral (c + (f' - c)) * (m' + (1 - m') * sigma / s)
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Calculate pool member reward
memberRew
  :: Coin
  -> PoolParams hashAlgo dsignAlgo
  -> StakeShare
  -> StakeShare
  -> Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = 0
  | otherwise = floor $ fromIntegral (f' - c) * (1 - m') * sigma / t
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Reward one pool
rewardOnePool
  :: PParams
  -> Coin
  -> Natural
  -> Natural
  -> StakeCredential hashAlgo dsignAlgo
  -> PoolParams hashAlgo dsignAlgo
  -> Stake hashAlgo dsignAlgo
  -> Coin
  -> Set.Set (RewardAcnt hashAlgo dsignAlgo)
  -> Map.Map (RewardAcnt hashAlgo dsignAlgo) Coin
rewardOnePool pp r blocksN blocksTotal poolHK pool (Stake stake) (Coin total) addrsRew =
  rewards'
  where
    Coin pstake = Map.foldl (+) (Coin 0) stake
    Coin ostake = stake Map.! poolHK
    sigma = fromIntegral pstake % fromIntegral total
    _expectedSlots = sigma * fromIntegral slotsPerEpoch
    Coin pledge = pool ^. poolPledge
    pr = fromIntegral pledge % fromIntegral total
    maxP =
      if pledge <= ostake
        then maxPool pp r sigma pr
        else 0
    s' = fromMaybe (error "LedgerState.rewardOnePool: Unexpected Nothing") $ mkUnitInterval sigma
    poolR = poolRewards poolHK s' blocksN blocksTotal maxP
    tot = fromIntegral total
    mRewards = Map.fromList
     [(RewardAcnt hk,
       memberRew poolR pool (StakeShare (fromIntegral c% tot)) (StakeShare sigma))
     | (hk, Coin c) <- Map.toList stake, hk /= poolHK]
    Coin hkStake = stake Map.! poolHK
    iReward  = leaderRew poolR pool (StakeShare $ fromIntegral hkStake % tot) (StakeShare sigma)
    potentialRewards = Map.insert (pool ^. poolRAcnt) iReward mRewards
    rewards' = addrsRew ◁ potentialRewards

reward
  :: PParams
  -> BlocksMade hashAlgo dsignAlgo
  -> Coin
  -> Set.Set (RewardAcnt hashAlgo dsignAlgo)
  -> Map.Map (KeyHash hashAlgo dsignAlgo) (PoolParams hashAlgo dsignAlgo)
  -> Stake hashAlgo dsignAlgo
  -> Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
  -> Map.Map (RewardAcnt hashAlgo dsignAlgo) Coin
reward pp (BlocksMade b) r addrsRew poolParams stake@(Stake stake') delegs =
  rewards'
  where
    total = Map.foldl (+) (Coin 0) stake'
    pdata =
      [ ( hk
        , ( poolParams Map.! hk
          , b Map.! hk
          , poolStake hk delegs stake))
      | hk <-
          Set.toList $ Map.keysSet poolParams `Set.intersection` Map.keysSet b
      ]
    results =
      [ ( hk
        , rewardOnePool pp r n totalBlocks (KeyHashObj hk) pool actgr total addrsRew)
      | (hk, (pool, n, actgr)) <- pdata
      ]
    rewards' = foldl (\m (_, r') -> Map.union m r') Map.empty results
    totalBlocks = Map.foldr (+) 0 b

-- | Stake distribution
stakeDistr
  :: UTxO hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
  -> PState hashAlgo dsignAlgo
  -> Stake hashAlgo dsignAlgo
stakeDistr u ds ps = Stake $ Map.keysSet activeDelegs ◁ stake
    where
      DState (StakeKeys stkeys) rewards' delegs ptrs' _ _ = ds
      PState (StakePools stpools) _ _ _                   = ps
      outs = consolidate u
      stake = baseStake' `Map.union` pointerStake `Map.union` rewardStake'
      Stake baseStake'   = baseStake outs
      Stake pointerStake = ptrStake outs ptrs'
      Stake rewardStake' = rewardStake rewards'
      activeDelegs       = Map.keysSet stkeys ◁ delegs ▷ Map.keysSet stpools

-- | Pool distribution
poolDistr
  :: UTxO hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
  -> PState hashAlgo dsignAlgo
  -> ( Stake hashAlgo dsignAlgo
     , Map.Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
     )
poolDistr u ds ps = (stake, delegs)
    where
      delegs     = ds ^. delegations
      stake      = stakeDistr u ds ps

-- | Apply a reward update
applyRUpd
  :: RewardUpdate hashAlgo dsignAlgo
  -> EpochState hashAlgo dsignAlgo
  -> EpochState hashAlgo dsignAlgo
applyRUpd ru (EpochState as ss ls pp) = es'
  where treasury' = _treasury as + deltaT ru
        reserves' = _reserves as + deltaR ru
        rew       = _rewards $ _dstate $ _delegationState ls
        rewards'  = rew ∪+ rs ru
        fees'     = _fees (_utxoState ls) + deltaF ru
        dstate'   = _dstate $ _delegationState ls
        utxo'     = _utxoState ls
        ls'       =
          ls { _utxoState = utxo' { _fees = fees' }
             , _delegationState = DPState
                  (dstate' { _rewards = rewards'})
                  (_pstate $ _delegationState ls)}
        es' = EpochState (AccountState treasury' reserves') ss ls' pp

-- | Create a reward update
createRUpd
  :: BlocksMade hashAlgo dsignAlgo
  -> EpochState hashAlgo dsignAlgo
  -> RewardUpdate hashAlgo dsignAlgo
createRUpd (BlocksMade b) (EpochState acnt ss ls pp) =
  RewardUpdate (Coin $ deltaT1 + deltaT2) (-deltaR') rs' (-(_feeSS ss))
  where Coin reserves' = _reserves acnt
        deltaR' =
          floor $ min 1 eta * intervalValue (_rho pp) * fromIntegral reserves'
        Coin totalPot = _feeSS ss + deltaR'
        deltaT1 = floor $ intervalValue (_tau pp) * fromIntegral totalPot
        r@(Coin r') = Coin $ totalPot - deltaT1
        rewards' = _rewards $ _dstate $ _delegationState ls
        (stake', delegs') = _pstakeGo ss
        poolsSS' = _poolsSS ss
        deltaT2 = r' - c'
        rs' = reward pp (_blocksSS ss) r (Map.keysSet rewards') poolsSS' stake' delegs'
        Coin c' = Map.foldr (+) (Coin 0) rs'
        blocksMade = fromIntegral $ Map.foldr (+) 0 b :: Integer
        expectedBlocks = intervalValue (_activeSlotCoeff pp) * fromIntegral slotsPerEpoch
        eta = fromIntegral blocksMade / expectedBlocks

-- | Overlay schedule
overlaySchedule
  :: Set (VKeyGenesis dsignAlgo)
  -> Seed
  -> PParams
  -> Map.Map Slot (Maybe (VKeyGenesis dsignAlgo))
overlaySchedule = undefined

-- | Set issue numbers
setIssueNumbers
  :: LedgerState hashAlgo dsignAlgo
  -> Map.Map (KeyHash hashAlgo dsignAlgo) Natural
  -> LedgerState hashAlgo dsignAlgo
setIssueNumbers (LedgerState u
                 (DPState _dstate
                  (PState stpools poolParams _retiring _ )) i) cs =
  LedgerState u (DPState _dstate (PState stpools poolParams _retiring cs)) i

-- | Update new epoch state
updateNES
  :: NewEpochState hashAlgo dsignAlgo
  -> BlocksMade hashAlgo dsignAlgo
  -> LedgerState hashAlgo dsignAlgo
  -> NewEpochState hashAlgo dsignAlgo
updateNES (NewEpochState eL eta0 bprev _
           (EpochState acnt ss _ pp) ru pd osched) bcur ls =
  NewEpochState eL eta0 bprev bcur (EpochState acnt ss ls pp) ru pd osched
