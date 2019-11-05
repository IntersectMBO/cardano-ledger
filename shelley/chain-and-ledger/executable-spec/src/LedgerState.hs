{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , RewardAccounts
  , emptyRewardUpdate
  , EpochState(..)
  , emptyEpochState
  , emptyLedgerState
  , emptyUTxOState
  , clearPpup
  , dstate
  , pstate
  , ptrs
  , fGenDelegs
  , genDelegs
  , irwd
  , PState(..)
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
  , applyUTxOUpdate
  -- * Genesis State
  , genesisId
  , genesisCoins
  , genesisState
  -- * Validation
  , ValidationError (..)
  , minfee
  , txsize
  , validStakePoolRetire
  , validInputs
  , validNoReplay
  , validFee
  , validKeyRegistration
  , validKeyDeregistration
  , validStakeDelegation
  , preserveBalance
  , produced
  , consumed
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
  , stkCreds
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
  , applyRUpd
  , createRUpd
  --
  , NewEpochState(..)
  , NewEpochEnv(..)
  , overlaySchedule
  , getGKeys
  , updateNES
  ) where

import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad (foldM)
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ratio ((%))
import qualified Data.Sequence as Seq (Seq (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Delegation.Certificates (requiresVKeyWitness)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Lens.Micro (to, (%~), (&), (.~), (^.))
import           Lens.Micro.TH (makeLenses)

import           Address (mkRwdAcnt)
import           Coin (Coin (..))
import           EpochBoundary (BlocksMade (..), SnapShots (..), Stake (..), aggregateOuts,
                     baseStake, emptySnapShots, maxPool, poolRefunds, poolStake, ptrStake,
                     rewardStake)
import           Keys (AnyKeyHash, DSIGNAlgorithm, GenDelegs (..), GenKeyHash, HashAlgorithm,
                     KeyDiscriminator (..), KeyHash, KeyPair, Signable, VRFAlgorithm, hash,
                     undiscriminateKeyHash)
import           PParams (PParams (..), activeSlotCoeff, d, emptyPParams, keyDecayRate, keyDeposit,
                     keyMinRefund, minfeeA, minfeeB)
import           Slot (Duration (..), Epoch (..), Slot (..), epochFromSlot, firstSlot,
                     slotsPerEpoch, (+*), (-*))
import           Tx (extractKeyHash)
import           TxData (Addr (..), Credential (..), Delegation (..), Ix, PoolParams, Ptr (..),
                     RewardAcnt (..), StakeCredential, Tx (..), TxBody (..), TxId (..), TxIn (..),
                     TxOut (..), body, certs, getRwdCred, inputs, poolOwners, poolPledge,
                     poolPubKey, poolRAcnt, ttl, txfee, wdrls, witKeyHash)
import           Updates (AVUpdate (..), PPUpdate (..), Update (..), UpdateState (..), emptyUpdate,
                     emptyUpdateState)
import           UTxO (UTxO (..), balance, deposits, txinLookup, txins, txouts, txup, verifyWitVKey)

import           Delegation.Certificates (DCert (..), PoolDistr (..), StakeCreds (..),
                     StakePools (..), cwitness, decayKey, refund)
import           Delegation.PoolParams (poolSpec)

import           BaseTypes (UnitInterval, intervalValue, mkUnitInterval)

import           Ledger.Core (dom, (∪), (∪+), (⋪), (▷), (◁))

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs dsignAlgo = [(KeyPair 'Regular dsignAlgo, KeyPair 'Regular dsignAlgo)]

-- | A ledger validation state consists of a ledger state 't' and the list of
-- validation errors that occurred from a valid 's' to reach 't'.
data LedgerValidation hashAlgo dsignAlgo vrfAlgo
  = LedgerValidation [ValidationError] (LedgerState hashAlgo dsignAlgo vrfAlgo)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerValidation hashAlgo dsignAlgo vrfAlgo)

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
    deriving (Show, Eq, Generic)

instance NoUnexpectedThunks ValidationError

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
  = Map (RewardAcnt hashAlgo dsignAlgo) Coin

-- | StakeShare type
newtype StakeShare =
  StakeShare Rational
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

-- | Construct an optional probability value
mkStakeShare :: Rational -> Maybe StakeShare
mkStakeShare p =
  if 0 <= p
    then Just $ StakeShare p
    else Nothing

data DState hashAlgo dsignAlgo = DState
    {  -- |The active stake keys.
      _stkCreds    :: StakeCreds hashAlgo dsignAlgo
      -- |The active accounts.
    ,  _rewards    :: RewardAccounts hashAlgo dsignAlgo
      -- |The current delegations.
    , _delegations :: Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
      -- |The pointed to hash keys.
    , _ptrs        :: Map Ptr (StakeCredential hashAlgo dsignAlgo)
      -- | future genesis key delegations
    , _fGenDelegs  :: Map (Slot, GenKeyHash hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
      -- |Genesis key delegations
    , _genDelegs   :: GenDelegs hashAlgo dsignAlgo
      -- | Instantaneous Rewards
    , _irwd        :: Map (Credential hashAlgo dsignAlgo) Coin
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DState hashAlgo dsignAlgo)

data PState hashAlgo dsignAlgo vrfAlgo = PState
    { -- |The active stake pools.
      _stPools     :: StakePools hashAlgo dsignAlgo
      -- |The pool parameters.
    , _pParams     :: Map (KeyHash hashAlgo dsignAlgo) (PoolParams hashAlgo dsignAlgo vrfAlgo)
      -- |A map of retiring stake pools to the epoch when they retire.
    , _retiring    :: Map (KeyHash hashAlgo dsignAlgo) Epoch
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PState hashAlgo dsignAlgo vrfAlgo)

-- |The state associated with the current stake delegation.
data DPState hashAlgo dsignAlgo vrfAlgo =
    DPState
    {
      _dstate :: DState hashAlgo dsignAlgo
    , _pstate :: PState hashAlgo dsignAlgo vrfAlgo
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DPState hashAlgo dsignAlgo vrfAlgo)

data RewardUpdate hashAlgo dsignAlgo = RewardUpdate
  { deltaT        :: Coin
  , deltaR        :: Coin
  , rs            :: Map (RewardAcnt hashAlgo dsignAlgo) Coin
  , deltaF        :: Coin
  , updateIRwd    :: Map (Credential hashAlgo dsignAlgo) Coin
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (RewardUpdate hashAlgo dsignAlgo)

emptyRewardUpdate :: RewardUpdate hashAlgo dsignAlgo
emptyRewardUpdate = RewardUpdate (Coin 0) (Coin 0) Map.empty (Coin 0) Map.empty

data AccountState = AccountState
  { _treasury  :: Coin
  , _reserves  :: Coin
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks AccountState

data EpochState hashAlgo dsignAlgo vrfAlgo
  = EpochState
    { esAccountState :: AccountState
    , esSnapshots :: SnapShots hashAlgo dsignAlgo vrfAlgo
    , esLState :: LedgerState hashAlgo dsignAlgo vrfAlgo
    , esPp :: PParams
    }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (EpochState hashAlgo dsignAlgo vrfAlgo)

emptyUTxOState :: UTxOState hashAlgo dsignAlgo vrfAlgo
emptyUTxOState = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

emptyEpochState :: EpochState hashAlgo dsignAlgo vrfAlgo
emptyEpochState =
  EpochState emptyAccount emptySnapShots emptyLedgerState  emptyPParams

emptyLedgerState :: LedgerState hashAlgo dsignAlgo vrfAlgo
emptyLedgerState =
  LedgerState
  emptyUTxOState
  emptyDelegation
  0

emptyAccount :: AccountState
emptyAccount = AccountState (Coin 0) (Coin 0)

emptyDelegation :: DPState hashAlgo dsignAlgo vrfAlgo
emptyDelegation =
    DPState emptyDState emptyPState

emptyDState :: DState hashAlgo dsignAlgo
emptyDState =
  DState (StakeCreds Map.empty) Map.empty Map.empty Map.empty Map.empty (GenDelegs Map.empty) Map.empty

emptyPState :: PState hashAlgo dsignAlgo vrfAlgo
emptyPState =
  PState (StakePools Map.empty) Map.empty Map.empty

-- |Clear the protocol parameter updates
clearPpup
  :: UTxOState hashAlgo dsignAlgo vrfAlgo
  -> UTxOState hashAlgo dsignAlgo vrfAlgo
clearPpup utxoSt =
  let UpdateState _ avup faps aps = _ups utxoSt
  in utxoSt {_ups = UpdateState (PPUpdate Map.empty) avup faps aps}

data UTxOState hashAlgo dsignAlgo vrfAlgo =
    UTxOState
    { _utxo      :: !(UTxO hashAlgo dsignAlgo vrfAlgo)
    , _deposited :: Coin
    , _fees      :: Coin
    , _ups       :: UpdateState hashAlgo dsignAlgo
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (UTxOState hashAlgo dsignAlgo vrfAlgo)

-- | New Epoch state and environment
data NewEpochState hashAlgo dsignAlgo vrfAlgo =
  NewEpochState {
    nesEL    :: Epoch
  , nesBprev :: BlocksMade hashAlgo dsignAlgo
  , nesBcur  :: BlocksMade hashAlgo dsignAlgo
  , nesEs    :: EpochState hashAlgo dsignAlgo vrfAlgo
  , nesRu    :: Maybe (RewardUpdate hashAlgo dsignAlgo)
  , nesPd    :: PoolDistr hashAlgo dsignAlgo vrfAlgo
  , nesOsched :: Map Slot (Maybe (GenKeyHash hashAlgo dsignAlgo))
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochState hashAlgo dsignAlgo vrfAlgo)

getGKeys
  :: NewEpochState hashAlgo dsignAlgo vrfAlgo
  -> Set (GenKeyHash hashAlgo dsignAlgo)
getGKeys nes = Map.keysSet genDelegs
  where NewEpochState _ _ _ es _ _ _ = nes
        EpochState _ _ ls _ = es
        LedgerState _ (DPState (DState _ _ _ _ _ (GenDelegs genDelegs) _) _) _ = ls

data NewEpochEnv hashAlgo dsignAlgo =
  NewEpochEnv {
    neeS     :: Slot
  , neeGkeys :: Set (GenKeyHash hashAlgo dsignAlgo)
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochEnv hashAlgo dsignAlgo)

-- |The state associated with a 'Ledger'.
data LedgerState hashAlgo dsignAlgo vrfAlgo =
  LedgerState
  { -- |The current unspent transaction outputs.
    _utxoState         :: !(UTxOState hashAlgo dsignAlgo vrfAlgo)
    -- |The current delegation state
  , _delegationState   :: !(DPState hashAlgo dsignAlgo vrfAlgo)
    -- |The current transaction index in the current slot.
  , _txSlotIx          :: Ix
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerState hashAlgo dsignAlgo vrfAlgo)

makeLenses ''DPState
makeLenses ''DState
makeLenses ''PState
makeLenses ''UTxOState
makeLenses ''AccountState
makeLenses ''LedgerState

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => TxId hashAlgo dsignAlgo vrfAlgo
genesisId =
  TxId $ hash
  (TxBody
   Set.empty
   []
   Seq.Empty
   Map.empty
   (Coin 0)
   (Slot 0)
   emptyUpdate)

-- |Creates the UTxO for a new ledger with the specified transaction outputs.
genesisCoins
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => [TxOut hashAlgo dsignAlgo]
  -> UTxO hashAlgo dsignAlgo vrfAlgo
genesisCoins outs = UTxO $
  Map.fromList [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs]

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState
  :: UTxO hashAlgo dsignAlgo vrfAlgo
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
genesisState utxo0 = LedgerState
  (UTxOState
    utxo0
    (Coin 0)
    (Coin 0)
    emptyUpdateState)
  emptyDelegation
  0

-- | Determine if the transaction has expired
current :: TxBody hashAlgo dsignAlgo vrfAlgo -> Slot -> Validity
current tx slot =
    if tx ^. ttl < slot
    then Invalid [Expired (tx ^. ttl) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: TxBody hashAlgo dsignAlgo vrfAlgo -> Validity
validNoReplay tx =
    if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs
  :: TxBody hashAlgo dsignAlgo vrfAlgo
  -> UTxOState hashAlgo dsignAlgo vrfAlgo
  -> Validity
validInputs tx u =
  if txins tx `Set.isSubsetOf` dom (u ^. utxo)
    then Valid
    else Invalid [BadInputs]

-- |Implementation of abstract transaction size
txsize :: TxBody hashAlgo dsignAlgo vrfAlgo -> Integer
txsize = toEnum . length . show

-- |Minimum fee calculation
minfee :: PParams -> TxBody hashAlgo dsignAlgo vrfAlgo -> Coin
minfee pc tx = Coin $ pc ^. minfeeA * txsize tx + fromIntegral (pc ^. minfeeB)

-- |Determine if the fee is large enough
validFee :: PParams -> TxBody hashAlgo dsignAlgo vrfAlgo -> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee pc tx
        given  = tx ^. txfee

-- |Compute the lovelace which are created by the transaction
produced
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => PParams
  -> StakePools hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> Coin
produced pp stakePools tx =
    balance (txouts tx) + tx ^. txfee + deposits pp stakePools (toList $ tx ^. certs)

-- |Compute the key deregistration refunds in a transaction
keyRefunds
  :: PParams
  -> StakeCreds hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> Coin
keyRefunds pp stk tx =
  sum [keyRefund dval dmin lambda stk (tx ^. ttl) c | c@(DeRegKey _) <- toList $ tx ^. certs]
  where (dval, dmin, lambda) = decayKey pp

-- | Key refund for a deregistration certificate.
keyRefund
  :: Coin
  -> UnitInterval
  -> Rational
  -> StakeCreds hashAlgo dsignAlgo
  -> Slot
  -> DCert hashAlgo dsignAlgo vrfAlgo
  -> Coin
keyRefund dval dmin lambda (StakeCreds stkcreds) slot c =
    case c of
      DeRegKey key -> case Map.lookup key stkcreds of
                        Nothing -> Coin 0
                        Just  s -> refund dval dmin lambda $ slot -* s
      _ -> Coin 0

-- | Functions to calculate decayed deposits
decayedKey
  :: PParams
  -> StakeCreds hashAlgo dsignAlgo
  -> Slot
  -> DCert hashAlgo dsignAlgo vrfAlgo
  -> Coin
decayedKey pp stk@(StakeCreds stkcreds) cslot cert =
    case cert of
      DeRegKey key ->
          if Map.notMember key stkcreds
          then 0
          else let created'      = stkcreds Map.! key in
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
  -> StakeCreds hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> Coin
decayedTx pp stk tx =
    sum [decayedKey pp stk (tx ^. ttl) c | c@(DeRegKey _) <- toList $ tx ^. certs]

-- |Compute the lovelace which are destroyed by the transaction
consumed
  :: PParams
  -> UTxO hashAlgo dsignAlgo vrfAlgo
  -> StakeCreds hashAlgo dsignAlgo
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> Coin
consumed pp u stakeKeys tx =
    balance (txins tx ◁ u) + refunds + withdrawals
  where
    refunds = keyRefunds pp stakeKeys tx
    withdrawals = sum $ tx ^. wdrls

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => StakePools hashAlgo dsignAlgo
  -> StakeCreds hashAlgo dsignAlgo
  -> PParams
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> UTxOState hashAlgo dsignAlgo vrfAlgo
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
  :: UTxO hashAlgo dsignAlgo vrfAlgo
  -> Tx hashAlgo dsignAlgo vrfAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> Set (AnyKeyHash hashAlgo dsignAlgo)
witsVKeyNeeded utxo' tx@(Tx txbody _ _) _genDelegs =
    inputAuthors `Set.union`
    wdrlAuthors  `Set.union`
    certAuthors  `Set.union`
    updateKeys   `Set.union`
    owners
  where
    inputAuthors = undiscriminateKeyHash `Set.map` Set.foldr insertHK Set.empty (txbody ^. inputs)
    insertHK txin hkeys =
      case txinLookup txin utxo' of
        Just (TxOut (AddrBase (KeyHashObj pay) _) _) -> Set.insert pay hkeys
        _                               -> hkeys

    wdrlAuthors =
      Set.fromList $ extractKeyHash $ map getRwdCred (Map.keys (txbody ^. wdrls))
    owners = foldl Set.union Set.empty
               [pool ^. poolOwners . to (Set.map undiscriminateKeyHash) | RegPool pool <- toList $ txbody ^. certs]
    certAuthors = Set.fromList $ extractKeyHash (fmap getCertHK certificates)
    getCertHK = cwitness
    certificates = filter requiresVKeyWitness (toList $ txbody ^. certs)
    updateKeys = undiscriminateKeyHash `Set.map` propWits (txup tx) _genDelegs

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are correct.
verifiedWits
  :: ( DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
  => Tx hashAlgo dsignAlgo vrfAlgo
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
  => Tx hashAlgo dsignAlgo vrfAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> UTxOState hashAlgo dsignAlgo vrfAlgo
  -> Validity
enoughWits tx@(Tx _ wits _) d' u =
  if witsVKeyNeeded (u ^. utxo) tx d' `Set.isSubsetOf` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = Set.map witKeyHash wits

validRuleUTXO
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => RewardAccounts hashAlgo dsignAlgo
  -> StakePools hashAlgo dsignAlgo
  -> StakeCreds hashAlgo dsignAlgo
  -> PParams
  -> Slot
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> UTxOState hashAlgo dsignAlgo vrfAlgo
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
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
  => Tx hashAlgo dsignAlgo vrfAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
  -> Validity
validRuleUTXOW tx d' l = verifiedWits tx
                   <> enoughWits tx d' (l ^. utxoState)

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits
  :: Update hashAlgo dsignAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> Set (KeyHash hashAlgo dsignAlgo)
propWits (Update (PPUpdate pup) (AVUpdate aup')) (GenDelegs _genDelegs) =
  Set.fromList $ Map.elems updateKeys
  where updateKeys = (Map.keysSet pup `Set.union` Map.keysSet aup') ◁ _genDelegs

validTx
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
  => Tx hashAlgo dsignAlgo vrfAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> Slot
  -> PParams
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
  -> Validity
validTx tx d' slot pp l =
    validRuleUTXO  (l ^. delegationState . dstate . rewards)
                   (l ^. delegationState . pstate . stPools)
                   (l ^. delegationState . dstate . stkCreds)
                   pp
                   slot
                   (tx ^. body)
                   (l ^. utxoState)
 <> validRuleUTXOW tx d' l

-- The rules for checking validiy of stake delegation transitions return
-- `certificate_type_correct(cert) -> valid_cert(cert)`, i.e., if the
-- certificate is of a different type, it's considered to be valid due to the
-- falsified hypothesis.

-- | Checks whether a key registration certificat is valid.
validKeyRegistration
  :: DCert hashAlgo dsignAlgo vrfAlgo
  -> DState hashAlgo dsignAlgo
  -> Validity
validKeyRegistration cert ds =
  case cert of
    RegKey key -> if not $ Map.member key stakeKeys
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
                   where (StakeCreds stakeKeys) = ds ^. stkCreds
    _          -> Valid

validKeyDeregistration
  :: DCert hashAlgo dsignAlgo vrfAlgo
  -> DState hashAlgo dsignAlgo
  -> Validity
validKeyDeregistration cert ds =
  case cert of
    DeRegKey key -> if Map.member key stakeKeys
                    then Valid else Invalid [StakeKeyNotRegistered]
                      where (StakeCreds stakeKeys) = ds ^. stkCreds
    _            -> Valid

validStakeDelegation
  ::  DCert hashAlgo dsignAlgo vrfAlgo
  -> DState hashAlgo dsignAlgo
  -> Validity
validStakeDelegation cert ds =
  case cert of
    Delegate (Delegation source _)
      -> if Map.member source stakeKeys
         then Valid else Invalid [StakeDelegationImpossible]
           where (StakeCreds stakeKeys) = ds ^. stkCreds
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister
  :: DCert hashAlgo dsignAlgo vrfAlgo
  -> DPState hashAlgo dsignAlgo vrfAlgo
  -> Validity
validStakePoolRegister _ _ = Valid

validStakePoolRetire
  :: DCert hashAlgo dsignAlgo vrfAlgo
  -> PState hashAlgo dsignAlgo vrfAlgo
  -> Validity
validStakePoolRetire cert ps =
  case cert of
    RetirePool key _ -> if Map.member key stakePools
                        then Valid else Invalid [StakePoolNotRegisteredOnKey]
                         where (StakePools stakePools) = ps ^. stPools
    _                -> Valid

validDelegation
  :: DCert hashAlgo dsignAlgo vrfAlgo
  -> DPState hashAlgo dsignAlgo vrfAlgo
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
     , VRFAlgorithm vrfAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
  => Slot
  -> PParams
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
  -> Tx hashAlgo dsignAlgo vrfAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> Either [ValidationError] (LedgerState hashAlgo dsignAlgo vrfAlgo)
asStateTransition slot pp ls tx d' =
  case validTx tx d' slot pp ls of
    Invalid errors -> Left errors
    Valid          -> foldM (certAsStateTransition slot (ls ^. txSlotIx)) ls' cs
      where
        ls' = applyTxBody ls pp (tx ^. body)
        cs = zip [0..] (toList $ tx ^. body . certs) -- index certificates

-- |In the case where a certificate is valid for a given ledger state,
-- apply the certificate as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
certAsStateTransition
  :: Slot
  -> Ix
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
  -> (Ix, DCert hashAlgo dsignAlgo vrfAlgo)
  -> Either [ValidationError] (LedgerState hashAlgo dsignAlgo vrfAlgo)
certAsStateTransition slot txIx ls (clx, cert) =
  case validDelegation cert (ls ^. delegationState) of
    Invalid errors -> Left errors
    Valid          -> Right $ ls & delegationState %~ applyDCert (Ptr slot txIx clx) cert

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     )
  => Slot
  -> PParams
  -> LedgerValidation hashAlgo dsignAlgo vrfAlgo
  -> Tx hashAlgo dsignAlgo vrfAlgo
  -> GenDelegs hashAlgo dsignAlgo
  -> LedgerValidation hashAlgo dsignAlgo vrfAlgo
asStateTransition' slot pp (LedgerValidation valErrors ls) tx d' =
    let ls' = applyTxBody ls pp (tx ^. body) in
    case validTx tx d' slot pp ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools
  :: LedgerState hashAlgo dsignAlgo vrfAlgo
  -> Epoch
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
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
  :: LedgerState hashAlgo dsignAlgo vrfAlgo
  -> PParams
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> Coin
depositPoolChange ls pp tx = (currentPool + txDeposits) - txRefunds
  -- Note that while (currentPool + txDeposits) >= txRefunds,
  -- it could be that txDeposits < txRefunds. We keep the parenthesis above
  -- to emphasize this point.
  where
    currentPool = ls ^. utxoState . deposited
    txDeposits =
      deposits pp (ls ^. delegationState . pstate . stPools) (toList $ tx ^. certs)
    txRefunds = keyRefunds pp (ls ^. delegationState . dstate . stkCreds) tx

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => LedgerState hashAlgo dsignAlgo vrfAlgo
  -> PParams
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
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
  :: (HashAlgorithm hashAlgo, DSIGNAlgorithm dsignAlgo, VRFAlgorithm vrfAlgo)
  => UTxOState hashAlgo dsignAlgo vrfAlgo
  -> TxBody hashAlgo dsignAlgo vrfAlgo
  -> UTxOState hashAlgo dsignAlgo vrfAlgo
applyUTxOUpdate u tx = u & utxo .~ txins tx ⋪ (u ^. utxo) ∪ txouts tx

-- |Apply a delegation certificate as a state transition function on the ledger state.
applyDCert
  :: Ptr
  -> DCert hashAlgo dsignAlgo vrfAlgo
  -> DPState hashAlgo dsignAlgo vrfAlgo
  -> DPState hashAlgo dsignAlgo vrfAlgo

applyDCert ptr dcert@(RegKey _) ds =
  ds & dstate %~ applyDCertDState ptr dcert

applyDCert ptr dcert@(DeRegKey _) ds =
  ds & dstate %~ applyDCertDState ptr dcert

applyDCert ptr dcert@(RegPool _) ds = ds & pstate %~ applyDCertPState ptr dcert

applyDCert ptr dcert@(RetirePool _ _) ds =
  ds & pstate %~ applyDCertPState ptr dcert

applyDCert _ (GenesisDelegate _) ds = ds -- TODO: check this

applyDCert _ (InstantaneousRewards _) _ = undefined

-- TODO do we also have to check hashKey target?
applyDCert ptr dcert@(Delegate _) ds =
  ds & dstate %~ applyDCertDState ptr dcert

applyDCertDState
  :: Ptr
  -> DCert hashAlgo dsignAlgo vrfAlgo
  -> DState hashAlgo dsignAlgo
  -> DState hashAlgo dsignAlgo
applyDCertDState (Ptr slot txIx clx) (DeRegKey key) ds =
    ds & stkCreds      .~ (StakeCreds $ Map.delete hksk stkcreds')
       & rewards     %~ Map.delete (RewardAcnt hksk)
       & delegations %~ Map.delete hksk
       & ptrs        %~ Map.delete (Ptr slot txIx clx)
        where hksk = key
              (StakeCreds stkcreds') = ds ^. stkCreds

applyDCertDState (Ptr slot txIx clx) (RegKey key) ds =
    ds & stkCreds  .~ (StakeCreds $ Map.insert hksk slot stkcreds')
       & rewards %~ Map.insert (RewardAcnt hksk) (Coin 0)
       & ptrs    %~ Map.insert (Ptr slot txIx clx) hksk
        where hksk = key
              (StakeCreds stkcreds') = ds ^. stkCreds

applyDCertDState _ (Delegate (Delegation source target)) ds =
    ds & delegations %~ Map.insert source target

applyDCertDState _ _ ds = ds

applyDCertPState
  :: Ptr
  -> DCert hashAlgo dsignAlgo vrfAlgo
  -> PState hashAlgo dsignAlgo vrfAlgo
  -> PState hashAlgo dsignAlgo vrfAlgo
applyDCertPState (Ptr slot _ _ ) (RegPool sp) ps =
    ps & stPools  .~ (StakePools $ Map.insert hsk slot' pools)
       & pParams  %~ Map.insert hsk sp
       & retiring %~ Map.delete hsk
  where hsk = sp ^. poolPubKey
        (StakePools pools) = ps ^. stPools
        slot' = fromMaybe slot (Map.lookup hsk pools)

-- TODO check epoch (not in new doc atm.)
applyDCertPState _ (RetirePool key epoch) ps =
  ps & retiring %~ Map.insert key epoch

-- | Use onlt pool registration or retirement certificates
applyDCertPState _ _ ps = ps

-- |Compute how much stake each active stake pool controls.
delegatedStake
  :: LedgerState hashAlgo dsignAlgo vrfAlgo
  -> Map (KeyHash hashAlgo dsignAlgo) Coin
delegatedStake ls@(LedgerState _ ds _) = Map.fromListWith (+) delegatedOutputs
  where
    getOutputs (UTxO utxo') = Map.elems utxo'
    addStake delegs (TxOut (AddrBase _ (KeyHashObj hsk)) c) = do
      pool <- Map.lookup (KeyHashObj hsk) delegs
      return (pool, c)
    addStake _ (TxOut (AddrBase _ _) _) = undefined -- TODO: script addresses
    addStake _ (TxOut (AddrEnterprise _) _) = undefined -- TODO: script addresses
    addStake delegs (TxOut (AddrPtr _ ptr) c) = do
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
  :: UnitInterval
  -> UnitInterval
  -> Natural
  -> Natural
  -> Coin
  -> Coin
poolRewards d_ sigma blocksN blocksTotal (Coin maxP) =
  if intervalValue d_ < 0.8
    then floor (p * fromIntegral maxP)
    else 1
  where
    p = beta / intervalValue sigma
    beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)

-- | Calculate pool leader reward
leaderRew
  :: Coin
  -> PoolParams hashAlgo dsignAlgo vrfAlgo
  -> StakeShare
  -> StakeShare
  -> Coin
leaderRew f@(Coin f') pool (StakeShare s) (StakeShare sigma)
  | f' <= c = f
  | otherwise =
    Coin $ c + floor (fromIntegral (f' - c) * (m' + (1 - m') * s / sigma))
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Calculate pool member reward
memberRew
  :: Coin
  -> PoolParams hashAlgo dsignAlgo vrfAlgo
  -> StakeShare
  -> StakeShare
  -> Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = 0
  | otherwise = floor $ fromIntegral (f' - c) * (1 - m') * t / sigma
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
  -> PoolParams hashAlgo dsignAlgo vrfAlgo
  -> Stake hashAlgo dsignAlgo
  -> Coin
  -> Set (RewardAcnt hashAlgo dsignAlgo)
  -> Map (RewardAcnt hashAlgo dsignAlgo) Coin
rewardOnePool pp r blocksN blocksTotal poolHK pool (Stake stake) (Coin total) addrsRew =
  rewards'
  where
    Coin pstake = Map.foldl (+) (Coin 0) stake
    Coin ostake = Set.foldl
                    (\c o -> c + (stake Map.! KeyHashObj o))
                    (Coin 0)
                    (pool ^. poolOwners)
    sigma = fromIntegral pstake % fromIntegral total
    Coin pledge = pool ^. poolPledge
    pr = fromIntegral pledge % fromIntegral total
    maxP =
      if pledge <= ostake
        then maxPool pp r sigma pr
        else 0
    s' = fromMaybe (error "LedgerState.rewardOnePool: Unexpected Nothing") $ mkUnitInterval sigma
    poolR = poolRewards (_d pp) s' blocksN blocksTotal maxP
    tot = fromIntegral total
    mRewards = Map.fromList
     [(RewardAcnt hk,
       memberRew poolR pool (StakeShare (fromIntegral c% tot)) (StakeShare sigma))
     | (hk, Coin c) <- Map.toList stake, hk /= poolHK]
    iReward  = leaderRew poolR pool (StakeShare $ fromIntegral ostake % tot) (StakeShare sigma)
    potentialRewards = Map.insert (pool ^. poolRAcnt) iReward mRewards
    rewards' = addrsRew ◁ potentialRewards

reward
  :: PParams
  -> BlocksMade hashAlgo dsignAlgo
  -> Coin
  -> Set (RewardAcnt hashAlgo dsignAlgo)
  -> Map (KeyHash hashAlgo dsignAlgo) (PoolParams hashAlgo dsignAlgo vrfAlgo)
  -> Stake hashAlgo dsignAlgo
  -> Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
  -> Map (RewardAcnt hashAlgo dsignAlgo) Coin
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
  :: forall hashAlgo dsignAlgo vrfAlgo
   . UTxO hashAlgo dsignAlgo vrfAlgo
  -> DState hashAlgo dsignAlgo
  -> PState hashAlgo dsignAlgo vrfAlgo
  -> ( Stake hashAlgo dsignAlgo
     , Map (StakeCredential hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
     )
stakeDistr u ds ps = ( Stake $ dom activeDelegs ◁ aggregatePlus stakeRelation
                     , delegs)
    where
      DState (StakeCreds stkcreds) rewards' delegs ptrs' _ _ _ = ds
      PState (StakePools stpools) _ _                          = ps
      outs = aggregateOuts u

      stakeRelation :: [(StakeCredential hashAlgo dsignAlgo, Coin)]
      stakeRelation = baseStake outs ∪ ptrStake outs ptrs' ∪ rewardStake rewards'

      activeDelegs = dom stkcreds ◁ delegs ▷ dom stpools

      aggregatePlus = Map.fromListWith (+)

-- | Apply a reward update
applyRUpd
  :: RewardUpdate hashAlgo dsignAlgo
  -> EpochState hashAlgo dsignAlgo vrfAlgo
  -> EpochState hashAlgo dsignAlgo vrfAlgo
applyRUpd ru (EpochState as ss ls pp) = EpochState as' ss ls' pp
  where utxoState_ = _utxoState ls
        delegState = _delegationState ls
        dState = _dstate delegState

        as' = as { _treasury = _treasury as + deltaT ru
                 , _reserves = _reserves as + deltaR ru + nonDistributed
                 }
        ls' = ls { _utxoState =
                     utxoState_ { _fees = _fees utxoState_ + deltaF ru
                                , _deposited = _deposited utxoState_ }
                 , _delegationState =
                     delegState
                     {_dstate = dState
                                { _rewards = (_rewards dState ∪+ rs ru) ∪+ updateRwd
                                , _irwd = Map.empty
                                }}}
        StakeCreds stkcreds = _stkCreds dState
        (rewMir', unregistered) =
          Map.partitionWithKey (\cred _ -> cred `Map.member` stkcreds) $ updateIRwd ru
        nonDistributed = Map.foldl (+) (Coin 0) unregistered
        updateRwd = Map.mapKeys mkRwdAcnt rewMir'

-- | Create a reward update
createRUpd
  :: BlocksMade hashAlgo dsignAlgo
  -> EpochState hashAlgo dsignAlgo vrfAlgo
  -> RewardUpdate hashAlgo dsignAlgo
createRUpd b@(BlocksMade b') (EpochState acnt ss ls pp) =
  RewardUpdate (Coin $ deltaT1 + deltaT2) (-deltaR') rs' (-(_feeSS ss)) registered
  where Coin reserves' = _reserves acnt

        ds = _dstate $ _delegationState ls
        rewards' = _rewards ds
        (stake', delegs') = _pstakeGo ss
        poolsSS' = _poolsSS ss
        StakeCreds stkcreds = _stkCreds ds

        -- instantaneous rewards
        registered = Map.filterWithKey (\cred _ -> cred `Map.member` stkcreds) (_irwd ds)

        Coin rewardsMIR = Map.foldl (+) (Coin 0) registered
        reserves'' = reserves' - rewardsMIR

        -- reserves and rewards change
        deltaRl =
            (floor $ min 1 eta * intervalValue (_rho pp) * fromIntegral reserves'')
        deltaR' = deltaRl + Coin rewardsMIR
        eta = fromIntegral blocksMade / expectedBlocks

        Coin rewardPot = _feeSS ss + deltaRl
        deltaT1 = floor $ intervalValue (_tau pp) * fromIntegral rewardPot
        r@(Coin r') = Coin $ rewardPot - deltaT1

        deltaT2 = r' - c'
        rs' = reward pp b r (Map.keysSet rewards') poolsSS' stake' delegs'
        Coin c' = Map.foldr (+) (Coin 0) rs'

        blocksMade = fromIntegral $ Map.foldr (+) 0 b' :: Integer
        expectedBlocks = intervalValue (_activeSlotCoeff pp) * fromIntegral slotsPerEpoch

-- | Overlay schedule
-- This is just a very simple round-robin, evenly spaced schedule.
-- The real implementation should probably use randomization.
overlaySchedule
  :: Epoch
  -> Set (GenKeyHash hashAlgo dsignAlgo)
  -> PParams
  -> Map Slot (Maybe (GenKeyHash hashAlgo dsignAlgo))
overlaySchedule e gkeys pp = Map.union active inactive
  where
    numActive = dval * fromIntegral slotsPerEpoch
    dval = intervalValue $ pp ^. d
    dInv = 1 / dval
    asc = intervalValue $ pp ^. activeSlotCoeff

    toRelativeSlot x = (Duration . floor) (dInv * fromInteger x)
    toSlot x = firstSlot e +* toRelativeSlot x

    genesisSlots = [ toSlot x | x <-[0..(floor numActive)] ]

    numInactivePerActive = floor (asc * fromRational numActive) - 1
    activitySchedule =  cycle (True:replicate numInactivePerActive False)
    unassignedSched = zip activitySchedule genesisSlots

    active =
      Map.fromList $ fmap
        (\(gk,(_,s))->(s, Just gk))
        (zip (cycle (Set.toList gkeys)) (filter fst unassignedSched))
    inactive =
      Map.fromList $ fmap
        (\x -> (snd x, Nothing))
        (filter (not . fst) unassignedSched)

-- | Update new epoch state
updateNES
  :: NewEpochState hashAlgo dsignAlgo vrfAlgo
  -> BlocksMade hashAlgo dsignAlgo
  -> LedgerState hashAlgo dsignAlgo vrfAlgo
  -> NewEpochState hashAlgo dsignAlgo vrfAlgo
updateNES (NewEpochState eL bprev _
           (EpochState acnt ss _ pp) ru pd osched) bcur ls =
  NewEpochState eL bprev bcur (EpochState acnt ss ls pp) ru pd osched
