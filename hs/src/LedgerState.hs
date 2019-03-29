{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE EmptyDataDecls        #-}

{-|
Module      : LedgerState
Description : Operational Rules

This module implements the operation rules for treating UTxO transactions ('TxWits')
as state transformations on a ledger state ('LedgerState'),
as specified in /A Simplified Formal Specification of a UTxO Ledger/.
-}


module LedgerState
  ( LedgerState(..)
  , Ix
  , DWState(..)
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
  , rewardPot
  , PState(..)
  , avgs
  , cCounters
  , LedgerValidation(..)
  , KeyPairs
  , UTxOState(..)
  , StakeShare(..)
  , Avgs(..)
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
  , applyUTxOUpdate
  -- * Genesis State
  , genesisId
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
  , enoughWits
  , noUnneededWits
  -- lenses
  , utxoState
  , delegationState
  , pcs
  , current
  -- UTxOState
  , utxo
  , deposited
  , fees
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
  , movingAvg
  , poolRewards
  , leaderRew
  , memberRew
  , rewardOnePool
  , reward
  , updateAvgs
  , stakeDistr
  , poolDistr
  , applyRUpd
  , createRUpd
  ) where

import           Control.Monad           (foldM)
import           Crypto.Hash             (hash)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (mapMaybe, fromMaybe)
import           Numeric.Natural         (Natural)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Ratio

import           Lens.Micro              ((^.), (&), (.~), (%~))
import           Lens.Micro.TH           (makeLenses)

import           Coin                    (Coin (..))
import           Slot                    (Slot (..), Epoch (..), (-*),
                                          slotsPerEpoch, firstSlot, epochFromSlot)
import           Keys
import           UTxO
import           PParams                 (PParams(..), minfeeA, minfeeB,
                                                 intervalValue, movingAvgWeight,
                                                 movingAvgExp,
                                                 keyDeposit, minRefund,
                                                 decayRate, emptyPParams)
import           EpochBoundary

import           Delegation.Certificates (DCert (..), refund, getRequiredSigningKey, StakeKeys(..), StakePools(..), decayKey)
import           Delegation.PoolParams   (Delegation (..), PoolParams (..),
                                         poolPubKey, poolSpec, poolPledge,
                                         RewardAcnt(..), poolRAcnt, poolOwners)

import           NonIntegral ((***))
import           BaseTypes

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
  -- | Unknown reward account
  | IncorrectRewards
  -- | One of the transaction witnesses is invalid.
  | InvalidWitness
  -- | The transaction does not have the required witnesses.
  | MissingWitnesses
  -- | The transaction includes a redundant witness.
  | UnneededWitnesses
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

type RewardAccounts = Map.Map RewardAcnt Coin

-- | Performance moving averages
newtype Avgs =
  Avgs (Map.Map HashKey StakeShare)
        deriving (Show, Eq)

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

data DState = DState
    {  -- |The active stake keys.
      _stKeys      :: StakeKeys
      -- |The active accounts.
    ,  _rewards    :: RewardAccounts
      -- |The current delegations.
    , _delegations :: Map.Map HashKey HashKey
      -- |The pointed to hash keys.
    , _ptrs        :: Map.Map Ptr HashKey
    } deriving (Show, Eq)

data PState = PState
    { -- |The active stake pools.
      _stPools     :: StakePools
      -- |The pool parameters.
    , _pParams     :: Map.Map HashKey PoolParams
      -- |A map of retiring stake pools to the epoch when they retire.
    , _retiring    :: Map.Map HashKey Epoch
      -- |Moving average for key in epoch.
    , _avgs        :: Avgs
      -- | Operational Certificate Counters.
    , _cCounters   :: Map.Map HashKey Natural
    } deriving (Show, Eq)

-- |The state associated with the current stake delegation.
data DWState =
    DWState
    {
      _dstate :: DState
    , _pstate :: PState
    } deriving (Show, Eq)

data RewardUpdate = RewardUpdate
  { deltaT :: Coin
  , deltaR :: Coin
  , rp     :: Coin
  , rs     :: Map.Map RewardAcnt Coin
  , deltaF :: Coin
  } deriving (Show, Eq)

emptyRewardUpdate :: RewardUpdate
emptyRewardUpdate = RewardUpdate (Coin 0) (Coin 0) (Coin 0) Map.empty (Coin 0)

data AccountState = AccountState
  { _treasury  :: Coin
  , _reserves  :: Coin
  , _rewardPot :: Coin
  } deriving (Show, Eq)

data EpochState = EpochState AccountState PParams SnapShots LedgerState
  deriving (Show, Eq)

emptyEpochState :: EpochState
emptyEpochState =
  EpochState emptyAccount emptyPParams emptySnapShots emptyLedgerState

emptyLedgerState :: LedgerState
emptyLedgerState = LedgerState
                   (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0))
                   emptyDelegation
                   emptyPParams
                   0
                   (Slot 0)


emptyAccount :: AccountState
emptyAccount = AccountState (Coin 0) (Coin 0) (Coin 0)

emptyDelegation :: DWState
emptyDelegation =
    DWState emptyDState emptyPState

emptyDState :: DState
emptyDState = DState (StakeKeys Map.empty) Map.empty Map.empty Map.empty

emptyPState :: PState
emptyPState =
  PState (StakePools Map.empty) Map.empty Map.empty (Avgs Map.empty) Map.empty

data UTxOState =
    UTxOState
    {
      _utxo      :: !UTxO
    , _deposited :: Coin
    , _fees      :: Coin
    } deriving (Show, Eq)

-- |The state associated with a 'Ledger'.
data LedgerState =
  LedgerState
  { -- |The current unspent transaction outputs.
    _utxoState         :: !UTxOState
    -- |The current delegation state
  , _delegationState   :: !DWState
    -- |The current protocol constants.
  , _pcs               :: !PParams
    -- | The current transaction index in the current slot.
  , _txSlotIx          :: Ix
  , _currentSlot       :: Slot
  } deriving (Show, Eq)

makeLenses ''DWState
makeLenses ''DState
makeLenses ''PState
makeLenses ''UTxOState
makeLenses ''AccountState
makeLenses ''LedgerState

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [] [] Map.empty (Coin 0) (Slot 0))

-- |Creates the ledger state for an empty ledger which
-- contains the specified transaction outputs.
genesisState :: PParams -> [TxOut] -> LedgerState
genesisState pc outs = LedgerState
  (UTxOState
    (UTxO $ Map.fromList
              [(TxIn genesisId idx, out) | (idx, out) <- zip [0..] outs])
    (Coin 0)
    (Coin 0))
  emptyDelegation
  pc
  0
  (Slot 0)

-- | Determine if the transaction has expired
current :: Tx -> Slot -> Validity
current tx slot =
    if tx ^. ttl < slot
    then Invalid [Expired (tx ^. ttl) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: Tx -> Validity
validNoReplay tx =
    if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- |Determine if the inputs in a transaction are valid for a given ledger state.
validInputs :: Tx -> UTxOState -> Validity
validInputs tx u =
  if txins tx `Set.isSubsetOf` dom (u ^. utxo)
    then Valid
    else Invalid [BadInputs]

-- |Implementation of abstract transaction size
txsize :: Tx -> Integer
txsize = toEnum . length . show

-- |Minimum fee calculation
minfee :: PParams -> Tx -> Coin
minfee pc tx = Coin $ pc ^. minfeeA * txsize tx + pc ^. minfeeB

-- |Determine if the fee is large enough
validFee :: PParams -> Tx -> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee pc tx
        given  = tx ^. txfee

-- |Compute the lovelace which are created by the transaction
produced :: PParams -> StakePools -> Tx -> Coin
produced pp stakePools tx =
    balance (txouts tx) + tx ^. txfee + deposits pp stakePools (tx ^. certs)

-- |Compute the key deregistration refunds in a transaction
keyRefunds :: PParams -> StakeKeys -> Tx -> Coin
keyRefunds pp stk tx =
  sum [keyRefund dval dmin lambda stk (tx ^. ttl) c | c@(DeRegKey _) <- tx ^. certs]
  where (dval, dmin, lambda) = decayKey pp

-- | Key refund for a deregistration certificate.
keyRefund :: Coin -> UnitInterval -> Rational -> StakeKeys -> Slot -> DCert -> Coin
keyRefund dval dmin lambda (StakeKeys stkeys) slot c =
    case c of
      DeRegKey key -> case Map.lookup (hashKey key) stkeys of
                        Nothing -> Coin 0
                        Just  s -> refund dval dmin lambda $ slot -* s
      _ -> Coin 0

-- | Functions to calculate decayed deposits
decayedKey :: PParams -> StakeKeys -> Slot -> DCert -> Coin
decayedKey pp stk@(StakeKeys stkeys) cslot cert =
    case cert of
      DeRegKey key ->
          if Map.notMember (hashKey key) stkeys
          then 0
          else let created'      = stkeys Map.! hashKey key in
               let start         = max (firstSlot $ epochFromSlot cslot) created' in
               let dval          = pp ^. keyDeposit in
               let dmin          = pp ^. minRefund in
               let lambda        = pp ^. decayRate in
               let epochRefund   = keyRefund dval dmin lambda stk start cert in
               let currentRefund = keyRefund dval dmin lambda stk cslot cert in
               epochRefund - currentRefund
      _ -> 0

-- | Decayed deposit portions
decayedTx :: PParams -> StakeKeys -> Tx -> Coin
decayedTx pp stk tx =
    sum [decayedKey pp stk (tx ^. ttl) c | c@(DeRegKey _) <- tx ^. certs]

-- |Compute the lovelace which are destroyed by the transaction
consumed :: PParams -> UTxO -> StakeKeys -> Tx -> Coin
consumed pp u stakeKeys tx =
    balance (txins tx <| u) + refunds + withdrawals
  where
    refunds = keyRefunds pp stakeKeys tx
    withdrawals = sum $ tx ^. wdrls

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance :: StakePools -> StakeKeys -> PParams -> Tx -> UTxOState -> Validity
preserveBalance stakePools stakeKeys pp tx u =
  if destroyed' == created'
    then Valid
    else Invalid [ValueNotConserved destroyed' created']
  where
    destroyed' = consumed pp (u ^. utxo) stakeKeys tx
    created' = produced pp stakePools tx

-- |Determine if the reward witdrawals correspond
-- to the rewards in the ledger state
correctWithdrawals :: RewardAccounts -> RewardAccounts -> Validity
correctWithdrawals accs withdrawals =
  if withdrawals `Map.isSubmapOf` accs
    then Valid
    else Invalid [IncorrectRewards]

-- |Collect the set of hashes of keys that needs to sign a
-- given transaction. This set consists of the txin owners,
-- certificate authors, and withdrawal reward accounts.
witsNeeded :: UTxO -> Tx -> Set HashKey
witsNeeded utxo' tx =
    inputAuthors `Set.union` wdrlAuthors `Set.union` certAuthors `Set.union` owners
  where
    inputAuthors = Set.foldr insertHK Set.empty (tx ^. inputs)
    insertHK txin hkeys =
      case txinLookup txin utxo' of
        Just (TxOut (AddrTxin pay _) _) -> Set.insert pay hkeys
        _                               -> hkeys

    wdrlAuthors = Set.map getRwdHK (Map.keysSet (tx ^. wdrls))
    owners = foldl Set.union Set.empty [pool ^. poolOwners | RegPool pool <- tx ^. certs]
    certAuthors = Set.fromList (fmap getCertHK (tx ^. certs))
    getCertHK cert = hashKey $ getRequiredSigningKey cert


-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are correct.
verifiedWits :: TxWits -> Validity
verifiedWits (TxWits tx wits) =
  if all (verifyWit tx) wits
    then Valid
    else Invalid [InvalidWitness]

-- |Given a ledger state, determine if the UTxO witnesses in a given
-- transaction are sufficient.
-- We check that there are not more witnesses than inputs, if several inputs
-- from the same address are used, it is not strictly necessary to include more
-- than one witness.
enoughWits :: TxWits -> UTxOState -> Validity
enoughWits (TxWits tx wits) u =
  if witsNeeded (u ^. utxo) tx `Set.isSubsetOf` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = Set.map (\(Wit vkey _) -> hashKey vkey) wits

-- |Check that there are no redundant witnesses.
noUnneededWits :: TxWits -> UTxOState -> Validity
noUnneededWits (TxWits tx wits) u =
  if signers `Set.isSubsetOf` witsNeeded (u ^. utxo) tx
    then Valid
    else Invalid [UnneededWitnesses]
  where
    signers = Set.map (\(Wit vkey _) -> hashKey vkey) wits

validRuleUTXO ::
    RewardAccounts -> StakePools -> StakeKeys -> PParams -> Slot -> Tx -> UTxOState -> Validity
validRuleUTXO accs stakePools stakeKeys pc slot tx u =
                          validInputs tx u
                       <> current tx slot
                       <> validNoReplay tx
                       <> validFee pc tx
                       <> preserveBalance stakePools stakeKeys pc tx u
                       <> correctWithdrawals accs (tx ^. wdrls)

validRuleUTXOW :: TxWits -> LedgerState -> Validity
validRuleUTXOW tx l = verifiedWits tx
                   <> enoughWits tx (l ^. utxoState)
                   <> noUnneededWits tx (l ^. utxoState)

validTx :: TxWits -> Slot -> LedgerState -> Validity
validTx tx slot l =
    validRuleUTXO  (l ^. delegationState . dstate . rewards)
                   (l ^. delegationState . pstate . stPools)
                   (l ^. delegationState . dstate . stKeys)
                   (l ^. pcs)
                   slot
                   (tx ^. body)
                   (l ^. utxoState)
 <> validRuleUTXOW tx l

-- The rules for checking validiy of stake delegation transitions return
-- `certificate_type_correct(cert) -> valid_cert(cert)`, i.e., if the
-- certificate is of a different type, it's considered to be valid due to the
-- falsified hypothesis.

-- | Checks whether a key registration certificat is valid.
validKeyRegistration :: DCert -> DWState -> Validity
validKeyRegistration cert ds =
  case cert of
    RegKey key -> if not $ Map.member (hashKey key) stakeKeys
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
                   where (StakeKeys stakeKeys) = ds ^. dstate . stKeys
    _          -> Valid

validKeyDeregistration :: DCert -> DWState -> Validity
validKeyDeregistration cert ds =
  case cert of
    DeRegKey key -> if Map.member (hashKey key) stakeKeys
                    then Valid else Invalid [StakeKeyNotRegistered]
                      where (StakeKeys stakeKeys) = ds ^. dstate . stKeys
    _            -> Valid

validStakeDelegation :: DCert -> DWState -> Validity
validStakeDelegation cert ds =
  case cert of
    Delegate (Delegation source _)
      -> if Map.member (hashKey source) stakeKeys
         then Valid else Invalid [StakeDelegationImpossible]
           where (StakeKeys stakeKeys) = ds ^. dstate . stKeys
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister :: DCert -> DWState -> Validity
validStakePoolRegister _ _ = Valid

validStakePoolRetire :: DCert -> DWState -> Validity
validStakePoolRetire cert ds =
  case cert of
    RetirePool key _ -> if Map.member (hashKey key) stakePools
                        then Valid else Invalid [StakePoolNotRegisteredOnKey]
                         where (StakePools stakePools) = ds ^. pstate . stPools
    _                -> Valid

validDelegation :: DCert -> DWState -> Validity
validDelegation cert ds =
     validKeyRegistration cert ds
  <> validKeyDeregistration cert ds
  <> validStakeDelegation cert ds
  <> validStakePoolRegister cert ds
  <> validStakePoolRetire cert ds

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition
  :: Slot -> LedgerState -> TxWits -> Either [ValidationError] LedgerState
asStateTransition slot ls tx =
  case validTx tx slot ls of
    Invalid errors -> Left errors
    Valid          -> foldM (certAsStateTransition slot (ls ^. txSlotIx)) ls' cs
      where
        ls' = applyTxBody slot ls (tx ^. body)
        cs = zip [0..] (tx ^. body . certs) -- index certificates

-- |In the case where a certificate is valid for a given ledger state,
-- apply the certificate as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
certAsStateTransition
  :: Slot -> Ix -> LedgerState -> (Ix, DCert) -> Either [ValidationError] LedgerState
certAsStateTransition slot txIx ls (clx, cert) =
  case validDelegation cert (ls ^. delegationState) of
    Invalid errors -> Left errors
    Valid          -> Right $ ls & delegationState %~ applyDCert (Ptr slot txIx clx) cert

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: Slot -> LedgerValidation -> TxWits -> LedgerValidation
asStateTransition' slot (LedgerValidation valErrors ls) tx =
    let ls' = applyTxBody slot ls (tx ^. body) in
    case validTx tx slot ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Epoch -> LedgerState
retirePools ls@(LedgerState _ ds _ _ _) epoch =
    ls & delegationState .~
           (ds & pstate . stPools .~
                 (StakePools $ Map.filterWithKey
                        (\hk _ -> Map.notMember hk retiring')
                        stakePools)
               & pstate . retiring .~ active)
  where (active, retiring') = Map.partition (epoch /=) (ds ^. pstate . retiring)
        (StakePools stakePools) = ds ^. pstate . stPools

-- |Calculate the change to the deposit pool for a given transaction.
depositPoolChange :: LedgerState -> Tx -> Coin
depositPoolChange ls tx = (currentPool + txDeposits) - txRefunds
  -- Note that while (currentPool + txDeposits) >= txRefunds,
  -- it could be that txDeposits < txRefunds. We keep the parenthesis above
  -- to emphasize this point.
  where
    currentPool = ls ^. utxoState . deposited
    txDeposits = deposits (ls ^. pcs) (ls ^. delegationState . pstate . stPools) (tx ^. certs)
    txRefunds = keyRefunds (ls ^. pcs) (ls ^. delegationState . dstate . stKeys) tx

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: Slot -> LedgerState -> Tx -> LedgerState
applyTxBody slot ls tx =
    ls & utxoState %~ flip applyUTxOUpdate tx
       & utxoState . deposited .~ depositPoolChange ls tx
       & utxoState . fees .~ (tx ^. txfee) + (ls ^. utxoState . fees)
       & delegationState . dstate . rewards .~ newAccounts
       & txSlotIx  %~ (if slot == ls ^. currentSlot then (+1) else const (0::Natural))
       & currentSlot .~ slot
  where
    newAccounts = reapRewards (ls ^. delegationState . dstate. rewards) (tx ^. wdrls)

reapRewards :: RewardAccounts -> RewardAccounts -> RewardAccounts
reapRewards dStateRewards withdrawals =
    Map.mapWithKey removeRewards dStateRewards
    where removeRewards k v = if k `Map.member` withdrawals then Coin 0 else v

applyUTxOUpdate :: UTxOState -> Tx -> UTxOState
applyUTxOUpdate u tx = u & utxo .~ txins tx </| (u ^. utxo) `union` txouts tx

-- |Apply a delegation certificate as a state transition function on the ledger state.
applyDCert :: Ptr -> DCert -> DWState -> DWState
applyDCert (Ptr slot txIx clx) (RegKey key) ds =
    ds & dstate . stKeys  .~ (StakeKeys $ Map.insert hksk slot stkeys')
       & dstate . rewards %~ Map.insert (RewardAcnt hksk) (Coin 0)
       & dstate . ptrs    %~ Map.insert (Ptr slot txIx clx) hksk
        where hksk = hashKey key
              (StakeKeys stkeys') = ds ^. dstate . stKeys

applyDCert (Ptr slot txIx clx) (DeRegKey key) ds =
    ds & dstate . stKeys      .~ (StakeKeys $ Map.delete hksk stkeys')
       & dstate . rewards     %~ Map.delete (RewardAcnt hksk)
       & dstate . delegations %~ Map.delete hksk
       & dstate . ptrs        %~ Map.delete (Ptr slot txIx clx)
        where hksk = hashKey key
              (StakeKeys stkeys') = ds ^. dstate . stKeys

-- TODO do we also have to check hashKey target?
applyDCert _ (Delegate (Delegation source target)) ds =
    ds & dstate . delegations %~ Map.insert (hashKey source) (hashKey target)

applyDCert (Ptr slot _ _) (RegPool sp) ds =
    ds & pstate . stPools  .~ (StakePools $ Map.insert hsk slot' pools)
       & pstate . pParams  %~ Map.insert hsk sp
       & pstate . retiring %~ Map.delete hsk
  where hsk = hashKey $ sp ^. poolPubKey
        (StakePools pools) = ds ^. pstate . stPools
        slot' = fromMaybe slot (Map.lookup hsk pools)

-- TODO check epoch (not in new doc atm.)
applyDCert _ (RetirePool key epoch) ds =
    ds & pstate . retiring %~ Map.insert hk_sp epoch
  where hk_sp = hashKey key

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map.Map HashKey Coin
delegatedStake ls@(LedgerState _ ds _ _ _) = Map.fromListWith (+) delegatedOutputs
  where
    getOutputs (UTxO utxo') = Map.elems utxo'
    addStake delegs (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup hsk delegs
      return (pool, c)
    addStake delegs (TxOut (AddrPtr ptr) c) = do
      key  <- Map.lookup ptr $ ds ^. dstate . ptrs
      pool <- Map.lookup key delegs
      return (pool, c)
    outs = getOutputs $ ls ^. utxoState . utxo
    delegatedOutputs = mapMaybe (addStake $ ds ^. dstate . delegations) outs

---------------------------------
-- epoch boundary calculations --
---------------------------------

-- | Calulcate moving average
movingAvg :: PParams -> HashKey -> Natural -> Rational -> Avgs -> Rational
movingAvg pc hk n expectedSlots (Avgs averages) =
  let fraction = fromIntegral n / max expectedSlots 1
   in case Map.lookup hk averages of
        Nothing -> fraction
        Just (StakeShare prev) -> alpha * fraction + (1 - alpha) * prev
          where alpha = intervalValue $ pc ^. movingAvgWeight

-- | Calculate pool reward
poolRewards ::
     PParams
  -> HashKey
  -> Natural
  -> Rational
  -> Avgs
  -> Coin
  -> Coin
poolRewards pc hk n expectedSlots averages (Coin maxP) =
  floor $ e * fromIntegral maxP
  where
    avg = pc ^. movingAvgExp
    gamma = movingAvg pc hk n expectedSlots averages
    e = approxRational (fromRational avg *** fromRational gamma) fpEpsilon

-- | Calculate pool leader reward
leaderRew :: Coin -> PoolParams -> StakeShare -> StakeShare -> Coin
leaderRew f@(Coin f') pool (StakeShare s) (StakeShare sigma)
  | f' <= c = f
  | otherwise =
    floor $ fromIntegral (c + (f' - c)) * (m' + (1 - m') * sigma / s)
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Calculate pool member reward
memberRew :: Coin -> PoolParams -> StakeShare -> StakeShare -> Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = 0
  | otherwise = floor $ fromIntegral (f' - c) * (1 - m') * sigma / t
  where
    (Coin c, m, _) = poolSpec pool
    m' = intervalValue m

-- | Reward one pool
rewardOnePool ::
     PParams
  -> Coin
  -> Natural
  -> HashKey
  -> PoolParams
  -> Stake
  -> Avgs
  -> Coin
  -> Set.Set RewardAcnt
  -> (Map.Map RewardAcnt Coin, Coin)
rewardOnePool pp r n poolHK pool (Stake stake) averages (Coin total) addrsRew =
  (rewards', unrealized)
  where
    Coin pstake = Map.foldl (+) (Coin 0) stake
    Coin ostake = stake Map.! poolHK
    sigma = fromIntegral pstake % fromIntegral total
    expectedSlots = sigma * fromIntegral slotsPerEpoch
    Coin pledge = pool ^. poolPledge
    pr = fromIntegral pledge % fromIntegral total
    maxP =
      if pledge <= ostake
        then maxPool pp r sigma pr
        else 0
    poolR = poolRewards pp poolHK n expectedSlots averages maxP
    tot = fromIntegral total
    mRewards = Map.fromList
     [(RewardAcnt hk,
       memberRew poolR pool (StakeShare (fromIntegral c% tot)) (StakeShare sigma))
     | (hk, Coin c) <- Map.toList stake, hk /= poolHK]
    Coin hkStake = stake Map.! poolHK
    iReward  = leaderRew poolR pool (StakeShare $ fromIntegral hkStake % tot) (StakeShare sigma)
    potentialRewards = Map.insert (pool ^. poolRAcnt) iReward mRewards
    rewards' = Map.restrictKeys potentialRewards addrsRew
    unrealized = r - Map.foldl (+) (Coin 0) rewards'

reward ::
     PParams
  -> BlocksMade
  -> Coin
  -> Set.Set RewardAcnt
  -> Map.Map HashKey PoolParams
  -> Avgs
  -> Stake
  -> Map.Map HashKey HashKey
  -> (Map.Map RewardAcnt Coin, Coin)
reward pp (BlocksMade b) r addrsRew poolParams avgs' stake@(Stake stake') delegs =
  (rewards', unrealized)
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
        , rewardOnePool pp r n hk pool actgr avgs' total addrsRew)
      | (hk, (pool, n, actgr)) <- pdata
      ]
    unrealized = foldl (\s (_, (_, u)) -> s + u) (Coin 0) results
    rewards' = foldl (\m (_, (r', _)) -> Map.union m r') Map.empty results

-- | Update moving averages
updateAvgs ::
     PParams
  -> Avgs
  -> BlocksMade
  -> Map.Map HashKey Stake
  -> Avgs
updateAvgs pp avgs' (BlocksMade blocks) pooledStake =
    Avgs $ Map.mapWithKey
             (\hk n -> StakeShare $ movingAvg pp hk n (expected hk) avgs') blocks
    where
      (Coin tot)  = Map.foldl (+) (Coin 0) $ Map.map sumStake pooledStake
      sumStake (Stake s) = Map.foldl (+) (Coin 0) s
      getCoinVal (Coin c) = fromIntegral c :: Integer
      expected hk =
          case Map.lookup hk pooledStake of
            Just st -> getCoinVal (sumStake st) * fromIntegral slotsPerEpoch % fromIntegral tot
            Nothing -> 0

-- | Stake distribution
stakeDistr :: UTxO -> DState -> PState -> Stake
stakeDistr u ds ps = Stake $ Map.restrictKeys stake (Map.keysSet activeDelegs)
    where
      DState (StakeKeys stkeys) rewards' delegs ptrs' = ds
      PState (StakePools stpools) _ _ _ _             = ps
      outs = consolidate u
      stake = baseStake' `Map.union` pointerStake `Map.union` rewardStake'
      Stake baseStake'   = baseStake outs
      Stake pointerStake = ptrStake outs ptrs'
      Stake rewardStake' = rewardStake rewards'
      activeDelegs       = Map.filter
                 (`Set.member` Map.keysSet stpools)
                 (Map.restrictKeys delegs (Map.keysSet stkeys))

-- | Pool distribution
poolDistr :: UTxO -> DState -> PState -> (Stake, Map.Map HashKey HashKey)
poolDistr u ds ps = (stake, delegs)
    where
      delegs     = ds ^. delegations
      stake      = stakeDistr u ds ps

-- | Apply a reward update
applyRUpd :: RewardUpdate -> EpochState -> EpochState
applyRUpd ru (EpochState as pp ss ls) = es'
  where treasury' = _treasury as + deltaT ru
        reserves' = _reserves as + deltaR ru
        rew       = _rewards $ _dstate $ _delegationState ls
        rewards'  = Map.union (rs ru) rew  -- prefer rs
        fees'     = (_fees $ _utxoState ls) + deltaF ru
        dstate'   = _dstate $ _delegationState ls
        utxo'     = _utxoState ls
        ls'       =
          ls { _utxoState = utxo' { _fees = fees' }
             , _delegationState = DWState
                  (dstate' { _rewards = rewards'})
                  (_pstate $ _delegationState ls)}
        es' = EpochState (AccountState treasury' reserves' (rp ru)) pp ss ls'

-- | Create a reward update
createRUpd :: BlocksMade -> EpochState -> RewardUpdate
createRUpd _ (EpochState acnt pp ss ls) = -- TODO where should `b` be used?
  RewardUpdate (Coin $ deltaT1 + deltaT2) (-deltaR') (Coin rp') rs' (-(_feeSS ss))
  where Coin reserves' = _reserves acnt
        deltaR' = floor $ (intervalValue $ _rho pp) * fromIntegral reserves'
        Coin totalPot = (_feeSS ss) + (_rewardPot acnt) + deltaR'
        deltaT1 = floor $ (intervalValue $ _tau pp) * fromIntegral totalPot
        r@(Coin r') = Coin $ totalPot - deltaT1
        rewards' = _rewards $ _dstate $ _delegationState ls
        avgs' = _avgs $ _pstate $ _delegationState ls
        (stake', delegs') = _pstakeGo ss
        poolsSS' = _poolsSS ss
        (rs', Coin deltaT2) = reward pp (_blocksSS ss) r (Map.keysSet rewards') poolsSS' avgs' stake' delegs'
        Coin c' = Map.foldr (+) (Coin 0) rs'
        rp' = (r' - deltaT2) - c'
