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
  , KeyPairs
  , UTxOState(..)
  , StakeShare(..)
  , emptyAccount
  , emptyPState
  , emptyDState
  , poolRAcnt
  , treasury
  , reserves
  -- * state transitions
  , emptyDelegation
  , applyTxBody
  -- * Genesis State
  , genesisId
  , genesisCoins
  , genesisState
  -- * Validation
  , LedgerValidation(..)
  , minfee
  , txsize
  , validTx
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
  , decayedTx
  -- epoch boundary
  , memberRew
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

import           Address (mkRwdAcnt)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin (..))
import           Data.Foldable (toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))
import qualified Data.Sequence as Seq (Seq (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Delegation.Certificates (requiresVKeyWitness)
import           EpochBoundary (BlocksMade (..), SnapShots (..), Stake (..), aggregateOuts,
                     baseStake, emptySnapShots, maxPool, poolStake, ptrStake, rewardStake)
import           GHC.Generics (Generic)
import           Keys (AnyKeyHash, GenDelegs (..), GenKeyHash, KeyDiscriminator (..), KeyHash,
                     KeyPair, Signable, hash, undiscriminateKeyHash)
import           Lens.Micro (to, (%~), (&), (.~), (^.))
import           Lens.Micro.TH (makeLenses)
import           Numeric.Natural (Natural)
import           PParams (PParams (..), activeSlotCoeff, d, emptyPParams, keyDecayRate, keyDeposit,
                     keyMinRefund, minfeeA, minfeeB)
import           Slot (Duration (..), Epoch (..), Slot (..), epochFromSlot, firstSlot,
                     slotsPerEpoch, (+*), (-*))
import           Tx (extractKeyHash)
import           TxData (Addr (..), Credential (..), Ix, PoolParams, Ptr (..), RewardAcnt (..),
                     StakeCredential, Tx (..), TxBody (..), TxId (..), TxIn (..), TxOut (..), body,
                     certs, getRwdCred, inputs, poolOwners, poolPledge, poolRAcnt, ttl, txfee,
                     wdrls, witKeyHash)
import           Updates (AVUpdate (..), PPUpdate (..), Update (..), UpdateState (..), emptyUpdate,
                     emptyUpdateState)
import           UTxO (UTxO (..), balance, deposits, txinLookup, txins, txouts, txup, verifyWitVKey)
import           Validation

import           Delegation.Certificates (DCert (..), PoolDistr (..), StakeCreds (..),
                     StakePools (..), cwitness, decayKey, refund)
import           Delegation.PoolParams (poolSpec)

import           BaseTypes (UnitInterval, intervalValue, mkUnitInterval)

import           Ledger.Core (dom, (∪), (∪+), (⋪), (▷), (◁))

-- | Representation of a list of pairs of key pairs, e.g., pay and stake keys
type KeyPairs crypto = [(KeyPair 'Regular crypto, KeyPair 'Regular crypto)]

-- | A ledger validation state consists of a ledger state 't' and the list of
-- validation errors that occurred from a valid 's' to reach 't'.
data LedgerValidation crypto
  = LedgerValidation [ValidationError] (LedgerState crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerValidation crypto)

type RewardAccounts crypto
  = Map (RewardAcnt crypto) Coin

-- | StakeShare type
newtype StakeShare =
  StakeShare Rational
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

-- | State of staking pool delegations and rewards
data DState crypto = DState
    {  -- |The active stake keys.
      _stkCreds    :: StakeCreds           crypto
      -- |The active reward accounts.
    ,  _rewards    :: RewardAccounts       crypto
      -- |The current delegations.
    , _delegations :: Map (StakeCredential crypto) (KeyHash crypto)
      -- |The pointed to hash keys.
    , _ptrs        :: Map Ptr (StakeCredential crypto)
      -- | future genesis key delegations
    , _fGenDelegs  :: Map (Slot, GenKeyHash crypto) (KeyHash crypto)
      -- |Genesis key delegations
    , _genDelegs   :: GenDelegs crypto
      -- | Instantaneous Rewards
    , _irwd        :: Map (Credential crypto) Coin
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DState crypto)

-- | Current state of staking pools and their certificate counters.
data PState crypto = PState
    { -- |The active stake pools.
      _stPools     :: StakePools crypto
      -- |The pool parameters.
    , _pParams     :: Map (KeyHash crypto) (PoolParams crypto)
      -- |A map of retiring stake pools to the epoch when they retire.
    , _retiring    :: Map (KeyHash crypto) Epoch
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PState crypto)

-- | The state associated with the current stake delegation.
data DPState crypto =
    DPState
    {
      _dstate :: DState crypto
    , _pstate :: PState crypto
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (DPState crypto)

data RewardUpdate crypto= RewardUpdate
  { deltaT        :: Coin
  , deltaR        :: Coin
  , rs            :: Map (RewardAcnt crypto) Coin
  , deltaF        :: Coin
  , updateIRwd    :: Map (Credential crypto) Coin
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (RewardUpdate crypto)

emptyRewardUpdate :: RewardUpdate crypto
emptyRewardUpdate = RewardUpdate (Coin 0) (Coin 0) Map.empty (Coin 0) Map.empty

data AccountState = AccountState
  { _treasury  :: Coin
  , _reserves  :: Coin
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks AccountState

data EpochState crypto
  = EpochState
    { esAccountState :: AccountState
    , esSnapshots :: SnapShots crypto
    , esLState :: LedgerState crypto
    , esPp :: PParams
    }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (EpochState crypto)

emptyUTxOState :: UTxOState crypto
emptyUTxOState = UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState

emptyEpochState :: EpochState crypto
emptyEpochState =
  EpochState emptyAccount emptySnapShots emptyLedgerState  emptyPParams

emptyLedgerState :: LedgerState crypto
emptyLedgerState =
  LedgerState
  emptyUTxOState
  emptyDelegation
  0

emptyAccount :: AccountState
emptyAccount = AccountState (Coin 0) (Coin 0)

emptyDelegation :: DPState crypto
emptyDelegation =
    DPState emptyDState emptyPState

emptyDState :: DState crypto
emptyDState =
  DState (StakeCreds Map.empty) Map.empty Map.empty Map.empty Map.empty (GenDelegs Map.empty) Map.empty

emptyPState :: PState crypto
emptyPState =
  PState (StakePools Map.empty) Map.empty Map.empty

-- |Clear the protocol parameter updates
clearPpup
  :: UTxOState crypto
  -> UTxOState crypto
clearPpup utxoSt =
  let UpdateState _ avup faps aps = _ups utxoSt
  in utxoSt {_ups = UpdateState (PPUpdate Map.empty) avup faps aps}

data UTxOState crypto=
    UTxOState
    { _utxo      :: !(UTxO crypto)
    , _deposited :: Coin
    , _fees      :: Coin
    , _ups       :: UpdateState crypto
    } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (UTxOState crypto)

-- | New Epoch state and environment
data NewEpochState crypto=
  NewEpochState {
    nesEL     :: Epoch                       -- ^ Last epoch
  , nesBprev  :: BlocksMade          crypto  -- ^ Blocks made before current epoch
  , nesBcur   :: BlocksMade          crypto  -- ^ Blocks made in current epoch
  , nesEs     :: EpochState          crypto  -- ^ Epoch state before current
  , nesRu     :: Maybe (RewardUpdate crypto) -- ^ Possible reward update
  , nesPd     :: PoolDistr           crypto  -- ^ Stake distribution within the stake pool
  , nesOsched :: Map  Slot
                     (Maybe
                       (GenKeyHash   crypto))  -- ^ Overlay schedule for PBFT vs Praos
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochState crypto)

getGKeys
  :: NewEpochState crypto
  -> Set (GenKeyHash crypto)
getGKeys nes = Map.keysSet genDelegs
  where NewEpochState _ _ _ es _ _ _ = nes
        EpochState _ _ ls _ = es
        LedgerState _ (DPState (DState _ _ _ _ _ (GenDelegs genDelegs) _) _) _ = ls

data NewEpochEnv crypto=
  NewEpochEnv {
    neeS     :: Slot
  , neeGkeys :: Set (GenKeyHash crypto)
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (NewEpochEnv crypto)

-- |The state associated with a 'Ledger'.
data LedgerState crypto=
  LedgerState
  { -- |The current unspent transaction outputs.
    _utxoState         :: !(UTxOState crypto)
    -- |The current delegation state
  , _delegationState   :: !(DPState crypto)
    -- |The current transaction index in the current slot.
  , _txSlotIx          :: Ix
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (LedgerState crypto)

makeLenses ''DPState
makeLenses ''DState
makeLenses ''PState
makeLenses ''UTxOState
makeLenses ''AccountState
makeLenses ''LedgerState

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId
  :: (Crypto crypto)
  => TxId crypto
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
  :: (Crypto crypto)
  => [TxOut crypto]
  -> UTxO crypto
genesisCoins outs = UTxO $
  Map.fromList [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs]

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState
  :: Map (GenKeyHash crypto) (KeyHash crypto)
  -> UTxO crypto
  -> LedgerState crypto
genesisState genDelegs0 utxo0 = LedgerState
  (UTxOState
    utxo0
    (Coin 0)
    (Coin 0)
    emptyUpdateState)
  (DPState dState emptyPState)
  0
  where
    dState = DState (StakeCreds Map.empty)
                    Map.empty Map.empty Map.empty Map.empty
                    (GenDelegs genDelegs0)
                    Map.empty

-- | Determine if the transaction has expired
current :: TxBody crypto-> Slot -> Validity
current tx slot =
    if tx ^. ttl < slot
    then Invalid [Expired (tx ^. ttl) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: TxBody crypto-> Validity
validNoReplay tx =
    if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs
  :: TxBody crypto
  -> UTxOState crypto
  -> Validity
validInputs tx u =
  if txins tx `Set.isSubsetOf` dom (u ^. utxo)
    then Valid
    else Invalid [BadInputs]

-- |Implementation of abstract transaction size
txsize :: TxBody crypto-> Integer
txsize = toEnum . length . show

-- |Minimum fee calculation
minfee :: PParams -> TxBody crypto-> Coin
minfee pc tx = Coin $ pc ^. minfeeA * txsize tx + fromIntegral (pc ^. minfeeB)

-- |Determine if the fee is large enough
validFee :: PParams -> TxBody crypto-> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee pc tx
        given  = tx ^. txfee

-- |Compute the lovelace which are created by the transaction
produced
  :: (Crypto crypto)
  => PParams
  -> StakePools crypto
  -> TxBody crypto
  -> Coin
produced pp stakePools tx =
    balance (txouts tx) + tx ^. txfee + deposits pp stakePools (toList $ tx ^. certs)

-- |Compute the key deregistration refunds in a transaction
keyRefunds
  :: PParams
  -> StakeCreds crypto
  -> TxBody crypto
  -> Coin
keyRefunds pp stk tx =
  sum [keyRefund dval dmin lambda stk (tx ^. ttl) c | c@(DeRegKey _) <- toList $ tx ^. certs]
  where (dval, dmin, lambda) = decayKey pp

-- | Key refund for a deregistration certificate.
keyRefund
  :: Coin
  -> UnitInterval
  -> Rational
  -> StakeCreds crypto
  -> Slot
  -> DCert crypto
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
  -> StakeCreds crypto
  -> Slot
  -> DCert crypto
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
  -> StakeCreds crypto
  -> TxBody crypto
  -> Coin
decayedTx pp stk tx =
    sum [decayedKey pp stk (tx ^. ttl) c | c@(DeRegKey _) <- toList $ tx ^. certs]

-- |Compute the lovelace which are destroyed by the transaction
consumed
  :: PParams
  -> UTxO crypto
  -> StakeCreds crypto
  -> TxBody crypto
  -> Coin
consumed pp u stakeKeys tx =
    balance (txins tx ◁ u) + refunds + withdrawals
  where
    refunds = keyRefunds pp stakeKeys tx
    withdrawals = sum $ tx ^. wdrls

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance
  :: (Crypto crypto)
  => StakePools crypto
  -> StakeCreds crypto
  -> PParams
  -> TxBody crypto
  -> UTxOState crypto
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
  :: RewardAccounts crypto
  -> RewardAccounts crypto
  -> Validity
correctWithdrawals accs withdrawals =
  if withdrawals `Map.isSubmapOf` accs
    then Valid
    else Invalid [IncorrectRewards]

-- |Collect the set of hashes of keys that needs to sign a
-- given transaction. This set consists of the txin owners,
-- certificate authors, and withdrawal reward accounts.
witsVKeyNeeded
  :: UTxO crypto
  -> Tx crypto
  -> GenDelegs crypto
  -> Set (AnyKeyHash crypto)
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
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Tx crypto
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
  :: Crypto crypto
  => Tx crypto
  -> GenDelegs crypto
  -> UTxOState crypto
  -> Validity
enoughWits tx@(Tx _ wits _) d' u =
  if witsVKeyNeeded (u ^. utxo) tx d' `Set.isSubsetOf` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = Set.map witKeyHash wits

validRuleUTXO
  :: (Crypto crypto)
  => RewardAccounts crypto
  -> StakePools crypto
  -> StakeCreds crypto
  -> PParams
  -> Slot
  -> TxBody crypto
  -> UTxOState crypto
  -> Validity
validRuleUTXO accs stakePools stakeKeys pc slot tx u =
                          validInputs tx u
                       <> current tx slot
                       <> validNoReplay tx
                       <> validFee pc tx
                       <> preserveBalance stakePools stakeKeys pc tx u
                       <> correctWithdrawals accs (tx ^. wdrls)

validRuleUTXOW
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Tx crypto
  -> GenDelegs crypto
  -> LedgerState crypto
  -> Validity
validRuleUTXOW tx d' l = verifiedWits tx
                   <> enoughWits tx d' (l ^. utxoState)

-- | Calculate the set of hash keys of the required witnesses for update
-- proposals.
propWits
  :: Update crypto
  -> GenDelegs crypto
  -> Set (KeyHash crypto)
propWits (Update (PPUpdate pup) (AVUpdate aup')) (GenDelegs _genDelegs) =
  Set.fromList $ Map.elems updateKeys
  where updateKeys = (Map.keysSet pup `Set.union` Map.keysSet aup') ◁ _genDelegs

validTx
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Tx crypto
  -> GenDelegs crypto
  -> Slot
  -> PParams
  -> LedgerState crypto
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

-- Functions for stake delegation model

-- |Calculate the change to the deposit pool for a given transaction.
depositPoolChange
  :: LedgerState crypto
  -> PParams
  -> TxBody crypto
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
  :: (Crypto crypto)
  => LedgerState crypto
  -> PParams
  -> TxBody crypto
  -> LedgerState crypto
applyTxBody ls pp tx =
    ls & utxoState %~ flip applyUTxOUpdate tx
       & utxoState . deposited .~ depositPoolChange ls pp tx
       & utxoState . fees .~ (tx ^. txfee) + (ls ^. utxoState . fees)
       & delegationState . dstate . rewards .~ newAccounts
       & txSlotIx  %~ (+) 1
  where
    newAccounts = reapRewards (ls ^. delegationState . dstate. rewards) (tx ^. wdrls)

reapRewards
  :: RewardAccounts crypto
  -> RewardAccounts crypto
  -> RewardAccounts crypto
reapRewards dStateRewards withdrawals =
    Map.mapWithKey removeRewards dStateRewards
    where removeRewards k v = if k `Map.member` withdrawals then Coin 0 else v

applyUTxOUpdate
  :: (Crypto crypto)
  => UTxOState crypto
  -> TxBody crypto
  -> UTxOState crypto
applyUTxOUpdate u tx = u & utxo .~ txins tx ⋪ (u ^. utxo) ∪ txouts tx

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
  -> PoolParams crypto
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
  -> PoolParams crypto
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
  -> StakeCredential crypto
  -> PoolParams crypto
  -> Stake crypto
  -> Coin
  -> Set (RewardAcnt crypto)
  -> Map (RewardAcnt crypto) Coin
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
  -> BlocksMade crypto
  -> Coin
  -> Set (RewardAcnt crypto)
  -> Map (KeyHash crypto) (PoolParams crypto)
  -> Stake crypto
  -> Map (StakeCredential crypto) (KeyHash crypto)
  -> Map (RewardAcnt crypto) Coin
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
  :: forall crypto
   . UTxO crypto
  -> DState crypto
  -> PState crypto
  -> ( Stake crypto
     , Map (StakeCredential crypto) (KeyHash crypto)
     )
stakeDistr u ds ps = ( Stake $ dom activeDelegs ◁ aggregatePlus stakeRelation
                     , delegs)
    where
      DState (StakeCreds stkcreds) rewards' delegs ptrs' _ _ _ = ds
      PState (StakePools stpools) _ _                          = ps
      outs = aggregateOuts u

      stakeRelation :: [(StakeCredential crypto, Coin)]
      stakeRelation = baseStake outs ∪ ptrStake outs ptrs' ∪ rewardStake rewards'

      activeDelegs = dom stkcreds ◁ delegs ▷ dom stpools

      aggregatePlus = Map.fromListWith (+)

-- | Apply a reward update
applyRUpd
  :: RewardUpdate crypto
  -> EpochState crypto
  -> EpochState crypto
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
  :: BlocksMade crypto
  -> EpochState crypto
  -> RewardUpdate crypto
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
  -> Set (GenKeyHash crypto)
  -> PParams
  -> Map Slot (Maybe (GenKeyHash crypto))
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
  :: NewEpochState crypto
  -> BlocksMade crypto
  -> LedgerState crypto
  -> NewEpochState crypto
updateNES (NewEpochState eL bprev _
           (EpochState acnt ss _ pp) ru pd osched) bcur ls =
  NewEpochState eL bprev bcur (EpochState acnt ss ls pp) ru pd osched
