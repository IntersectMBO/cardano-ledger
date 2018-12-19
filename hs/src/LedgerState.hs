{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

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
  , minfee
  -- lenses
  , utxoState
  , delegationState
  , pcs
  -- UTxOState
  , utxo
  , deposits
  , fees
  -- DelegationState
  , rewards
  , stKeys
  , delegations
  , stPools
  , pParams
  , retiring
  ) where

import           Control.Monad           (foldM)
import           Crypto.Hash             (hash)
import qualified Data.Map                as Map
import           Data.Maybe              (mapMaybe, fromMaybe)
import           Numeric.Natural         (Natural)
import           Data.Set                (Set)
import qualified Data.Set                as Set

import           Lens.Micro              ((^.), (&), (.~), (%~))
import           Lens.Micro.TH           (makeLenses)

import           Coin                    (Coin (..))
import           Slot                    (Slot (..), Epoch (..), (-*))
import           Keys
import           UTxO
import           PrtlConsts              (PrtlConsts(..), minfeeA, minfeeB)

import           Delegation.Certificates (DCert (..), refund, getRequiredSigningKey)
import           Delegation.StakePool    (Delegation (..), StakePool (..), poolPubKey)

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

type Allocs = Map.Map HashKey Slot

type RewardAccounts = Map.Map RewardAcnt Coin

-- |The state associated with the current stake delegation.
data DelegationState =
    DelegationState
    {
    -- |The active accounts.
      _rewards     :: RewardAccounts
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

makeLenses ''DelegationState
makeLenses ''UTxOState
makeLenses ''LedgerState

-- |The transaction Id for 'UTxO' included at the beginning of a new ledger.
genesisId :: TxId
genesisId = TxId $ hash (Tx Set.empty [] [] Map.empty (Coin 0) (Slot 0))

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
txsize :: Tx -> Natural
txsize = toEnum . length . show

-- |Minimum fee calculation
minfee :: PrtlConsts -> Tx -> Coin
minfee pc tx = Coin $ pc ^. minfeeA * txsize tx + pc ^. minfeeB

-- |Determine if the fee is large enough
validFee :: PrtlConsts -> Tx -> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
      where
        needed = minfee pc tx
        given  = tx ^. txfee

-- |Compute the lovelace which are created by the transaction
created :: Allocs -> PrtlConsts -> Tx -> Coin
created stakePools pc tx =
    balance (txouts tx) + tx ^. txfee + depositAmount pc stakePools tx

-- |Compute the key deregistration refunds in a transaction
keyRefunds :: PrtlConsts -> Allocs -> Tx -> Coin
keyRefunds pc stkeys tx =
  sum [refund' key | (RegKey key) <- tx ^. certs]
  where refund' key =
          case Map.lookup (hashKey key) stkeys of
            Nothing -> Coin 0
            Just s -> refund (RegKey key) pc $ (tx ^. ttl) -* s

-- |Compute the lovelace which are destroyed by the transaction
destroyed :: Allocs -> PrtlConsts -> Tx -> UTxOState -> Coin
destroyed stakeKeys pc tx u =
    balance (txins tx <| (u ^. utxo)) + refunds + withdrawals
  where
    refunds = keyRefunds pc stakeKeys tx
    withdrawals = sum $ tx ^. wdrls

-- |Determine if the balance of the ledger state would be effected
-- in an acceptable way by a transaction.
preserveBalance :: Allocs -> Allocs -> PrtlConsts -> Tx -> UTxOState -> Validity
preserveBalance stakePools stakeKeys pc tx u =
  if destroyed' == created'
    then Valid
    else Invalid [ValueNotConserved destroyed' created']
  where
    destroyed' = destroyed stakeKeys pc tx u
    created' = created stakePools pc tx

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
requiredSigners :: Tx -> UTxO -> Set HashKey
requiredSigners tx utxo' = inputAuthors `Set.union` wdrlAuthors `Set.union` certAuthors
  where
    inputAuthors = Set.foldr insertHK Set.empty (tx ^. inputs)
    insertHK txin hkeys =
      case txinLookup txin utxo' of
        Just (TxOut (AddrTxin pay _) _) -> Set.insert pay hkeys
        _                               -> hkeys

    wdrlAuthors = Set.map getRwdHK (Map.keysSet (tx ^. wdrls))

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
  if requiredSigners tx (u ^. utxo) `Set.isSubsetOf` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = Set.map (\(Wit vkey _) -> hashKey vkey) wits

-- |Check that there are no redundant witnesses.
noUnneededWits :: TxWits -> UTxOState -> Validity
noUnneededWits (TxWits tx wits) u =
  if signers `Set.isSubsetOf` requiredSigners tx (u ^. utxo)
    then Valid
    else Invalid [UnneededWitnesses]
  where
    signers = Set.map (\(Wit vkey _) -> hashKey vkey) wits

validRuleUTXO ::
    RewardAccounts -> Allocs -> Allocs -> PrtlConsts -> Slot -> Tx -> UTxOState -> Validity
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
    validRuleUTXO  (l ^. delegationState . rewards)
                   (l ^. delegationState . stPools)
                   (l ^. delegationState . stKeys)
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
validKeyRegistration :: DCert -> DelegationState -> Validity
validKeyRegistration cert ds =
  case cert of
    RegKey key -> if not $ Map.member (hashKey key) (ds ^. stKeys)
                  then Valid else Invalid [StakeKeyAlreadyRegistered]
    _          -> Valid

validKeyDeregistration :: DCert -> DelegationState -> Validity
validKeyDeregistration cert ds =
  case cert of
    DeRegKey key -> if Map.member (hashKey key) (ds ^. stKeys)
                    then Valid else Invalid [StakeKeyNotRegistered]
    _            -> Valid

validStakeDelegation :: DCert -> DelegationState -> Validity
validStakeDelegation cert ds =
  case cert of
    Delegate (Delegation source _)
      -> if Map.member (hashKey source) (ds ^. stKeys)
         then Valid else Invalid [StakeDelegationImpossible]
    _ -> Valid

-- there is currently no requirement that could make this invalid
validStakePoolRegister :: DCert -> DelegationState -> Validity
validStakePoolRegister _ _ = Valid

validStakePoolRetire :: DCert -> DelegationState -> Validity
validStakePoolRetire cert ds =
  case cert of
    RetirePool key _ -> if Map.member (hashKey key) $ ds ^. stPools
                        then Valid else Invalid [StakePoolNotRegisteredOnKey]
    _                -> Valid

validDelegation :: DCert -> DelegationState -> Validity
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
    Valid          -> foldM (certAsStateTransition slot) ls' cs
      where
        ls' = applyTxBody ls (tx ^. body)
        cs = tx ^. body . certs

-- |In the case where a certificate is valid for a given ledger state,
-- apply the certificate as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
certAsStateTransition
  :: Slot -> LedgerState -> DCert -> Either [ValidationError] LedgerState
certAsStateTransition slot ls cert =
  case validDelegation cert (ls ^. delegationState) of
    Invalid errors -> Left errors
    Valid          -> Right $ ls & delegationState %~ (applyDCert slot cert)

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: Slot -> LedgerValidation -> TxWits -> LedgerValidation
asStateTransition' slot (LedgerValidation valErrors ls) tx =
    let ls' = applyTxBody ls (tx ^. body) in
    case validTx tx slot ls of
      Invalid errors -> LedgerValidation (valErrors ++ errors) ls'
      Valid          -> LedgerValidation valErrors ls'

-- Functions for stake delegation model

-- |Retire the appropriate stake pools when the epoch changes.
retirePools :: LedgerState -> Epoch -> LedgerState
retirePools ls@(LedgerState _ ds _) epoch =
    ls & delegationState .~
           (ds & stPools .~
                 Map.filterWithKey
                        (\hk _ -> Map.notMember hk retiring')
                        (ds ^. stPools)
               & retiring .~ active)
  where (active, retiring') = Map.partition (epoch /=) (ds ^. retiring)

-- |Calculate the change to the deposit pool for a given transaction.
depositPoolChange :: LedgerState -> Tx -> Coin
depositPoolChange ls tx = (currentPool + txDeposits) - txRefunds
  -- Note that while (currentPool + txDeposits) >= txRefunds,
  -- it could be that txDeposits < txRefunds. We keep the parenthesis above
  -- to emphasize this point.
  where
    currentPool = ls ^. utxoState . deposits
    txDeposits = depositAmount (ls ^. pcs) (ls ^. delegationState . stPools) tx
    txRefunds = keyRefunds (ls ^. pcs) (ls ^. delegationState . stKeys) tx

-- |Apply a transaction body as a state transition function on the ledger state.
applyTxBody :: LedgerState -> Tx -> LedgerState
applyTxBody ls tx = ls & utxoState %~ flip applyUTxOUpdate tx
                       & utxoState . deposits .~ depositPoolChange ls tx
                       & utxoState . fees .~ (tx ^. txfee) + (ls ^. utxoState . fees)
                       & delegationState . rewards .~ newAccounts
  where
    newAccounts = reapRewards (ls ^. delegationState . rewards) (tx ^. wdrls)

reapRewards :: RewardAccounts -> RewardAccounts -> RewardAccounts
reapRewards dStateRewards withdrawals =
    Map.mapWithKey removeRewards dStateRewards
    where removeRewards k v = if k `Map.member` withdrawals then Coin 0 else v

applyUTxOUpdate :: UTxOState -> Tx -> UTxOState
applyUTxOUpdate u tx = u & utxo .~ txins tx </| (u ^. utxo) `union` txouts tx

-- |Apply a delegation certificate as a state transition function on the ledger state.
applyDCert :: Slot -> DCert -> DelegationState -> DelegationState
applyDCert slot (RegKey key) ds =
    ds & stKeys  %~ Map.insert hksk slot
       & rewards %~ Map.insert (RewardAcnt hksk) (Coin 0)
        where hksk = hashKey key

applyDCert _ (DeRegKey key) ds =
    ds & stKeys      %~ Map.delete hksk
       & rewards     %~ Map.delete (RewardAcnt hksk)
       & delegations %~ Map.delete hksk
        where hksk = hashKey key

-- TODO do we also have to check hashKey target?
applyDCert _ (Delegate (Delegation source target)) ds =
    ds & delegations %~ Map.insert (hashKey source) (hashKey target)

applyDCert slot (RegPool sp) ds =
    ds & stPools  %~ Map.insert hsk slot'
       & pParams  %~ Map.insert hsk sp
       & retiring %~ Map.delete hsk
  where hsk = hashKey $ sp ^. poolPubKey
        pools = ds ^. stPools
        slot' = fromMaybe slot (Map.lookup hsk pools)

-- TODO check epoch (not in new doc atm.)
applyDCert _ (RetirePool key epoch) ds =
    ds & retiring %~ Map.insert hk_sp epoch
  where hk_sp = hashKey key

-- |Compute how much stake each active stake pool controls.
delegatedStake :: LedgerState -> Map.Map HashKey Coin
delegatedStake ls@(LedgerState _ ds _) = Map.fromListWith mappend delegatedOutputs
  where
    getOutputs (UTxO utxo') = Map.elems utxo'
    addStake delegs (TxOut (AddrTxin _ hsk) c) = do
      pool <- Map.lookup hsk delegs
      return (pool, c)
    outs = getOutputs $ ls ^. utxoState . utxo
    delegatedOutputs = mapMaybe (addStake $ ds ^. delegations) outs

---------------------------------------------------------------------------------
-- State transition system
---------------------------------------------------------------------------------

data UTXO

instance STS UTXO where
    type State UTXO       = UTxOState
    type Signal UTXO      = Tx
    type Environment UTXO = (PrtlConsts, Slot, Allocs, Allocs)
    data PredicateFailure UTXO = BadInputsUTxO
                               | ExpiredUTxO Slot Slot
                               | InputSetEmptyUTxO
                               | FeeTooSmallUTxO Coin Coin
                               | ValueNotConservedUTxO Coin Coin
                               | UnexpectedFailureUTXO [ValidationError]
                               | UnexpectedSuccessUTXO
                   deriving (Eq, Show)

    transitionRules = [ utxoInductive ]

    initialRules = [ initialLedgerState ]

initialLedgerState :: InitialRule UTXO
initialLedgerState = do
  IRC (_) <- judgmentContext
  pure $ UTxOState (UTxO Map.empty) (Coin 0) (Coin 0)

utxoInductive :: TransitionRule UTXO
utxoInductive = do
  TRC ((pc, slot, stakePools, stakeKeys), u, tx) <- judgmentContext
  validInputs tx u       == Valid ?! BadInputsUTxO
  current tx slot        == Valid ?! ExpiredUTxO (tx ^. ttl) slot
  validNoReplay tx       == Valid ?! InputSetEmptyUTxO
  let validateFee         = validFee pc tx
  validateFee            == Valid ?! unwrapFailureUTXO validateFee
  let validateBalance     = preserveBalance stakePools stakeKeys pc tx u
  validateBalance        == Valid ?! unwrapFailureUTXO validateBalance
  pure $ applyUTxOUpdate u tx

unwrapFailureUTXO :: Validity -> PredicateFailure UTXO
unwrapFailureUTXO (Invalid [e]) = unwrapFailureUTXO' e
unwrapFailureUTXO Valid         = UnexpectedSuccessUTXO
unwrapFailureUTXO (Invalid x)   = UnexpectedFailureUTXO x

unwrapFailureUTXO' :: ValidationError -> PredicateFailure UTXO
unwrapFailureUTXO' BadInputs                = BadInputsUTxO
unwrapFailureUTXO' (Expired s s')           = ExpiredUTxO s s'
unwrapFailureUTXO' InputSetEmpty            = InputSetEmptyUTxO
unwrapFailureUTXO' (FeeTooSmall c c')       = FeeTooSmallUTxO c c'
unwrapFailureUTXO' (ValueNotConserved c c') = ValueNotConservedUTxO c c'
unwrapFailureUTXO' x                        = UnexpectedFailureUTXO [x]

data UTXOW

instance STS UTXOW where
    type State UTXOW       = UTxOState
    type Signal UTXOW      = TxWits
    type Environment UTXOW = (PrtlConsts, Slot, Allocs, Allocs)
    data PredicateFailure UTXOW = InvalidWitnessesUTXOW
                                | MissingWitnessesUTXOW
                                | UnneededWitnessesUTXOW
                                | UtxoFailure (PredicateFailure UTXO)
                   deriving (Eq, Show)

    transitionRules = [ utxoWitnessed ]

    initialRules = [ initialLedgerStateUTXOW ]

initialLedgerStateUTXOW :: InitialRule UTXOW
initialLedgerStateUTXOW = do
  IRC ((pc, slots, stakePools, stakeKeys)) <- judgmentContext
  trans @UTXO $ IRC ((pc, slots, stakePools, stakeKeys))


utxoWitnessed :: TransitionRule UTXOW
utxoWitnessed = do
  TRC ((pc, slot, stakePools, stakeKeys), u, txwits) <- judgmentContext
  verifiedWits txwits     == Valid ?! InvalidWitnessesUTXOW
  enoughWits txwits u     == Valid ?! MissingWitnessesUTXOW
  noUnneededWits txwits u == Valid ?! UnneededWitnessesUTXOW
  trans @UTXO $ TRC ((pc, slot, stakePools, stakeKeys), u, txwits ^. body)

instance Embed UTXO UTXOW where
    wrapFailed = UtxoFailure

data DELRWDS

instance STS DELRWDS where
    type State DELRWDS            = DelegationState
    type Signal DELRWDS           = RewardAccounts
    type Environment DELRWDS      = Slot
    data PredicateFailure DELRWDS = IncorrectWithdrawalDELRWDS
                     deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delrwdsTransition ]

delrwdsTransition :: TransitionRule DELRWDS
delrwdsTransition = do
  TRC (_, d, withdrawals) <- judgmentContext
  correctWithdrawals (d ^. rewards) withdrawals == Valid ?! IncorrectWithdrawalDELRWDS
  pure $ d & rewards .~ (reapRewards (d ^. rewards) withdrawals)


data DELEG

instance STS DELEG where
    type State DELEG            = DelegationState
    type Signal DELEG           = DCert
    type Environment DELEG      = Slot
    data PredicateFailure DELEG = StakeKeyAlreadyRegisteredDELEG
                                | StakeKeyNotRegisteredDELEG
                                | StakeDelegationImpossibleDELEG
                                | WrongCertificateTypeDELEG
                   deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delegationTransition ]

delegationTransition :: TransitionRule DELEG
delegationTransition = do
  TRC(slot, d, c) <- judgmentContext
  case c of
    RegKey _   -> do
           validKeyRegistration c d == Valid ?! StakeKeyAlreadyRegisteredDELEG
           pure $ applyDCert slot c d
    DeRegKey _ -> do
           validKeyDeregistration c d == Valid ?! StakeKeyNotRegisteredDELEG
           pure $ applyDCert slot c d
    Delegate _ -> do
           validStakeDelegation c d == Valid ?! StakeDelegationImpossibleDELEG
           pure $ applyDCert slot c d
    _         -> do
           False ?! WrongCertificateTypeDELEG -- this always fails
           pure d

data POOL

instance STS POOL where
    type State POOL         = (DelegationState)
    type Signal POOL        = DCert
    type Environment POOL   = Slot
    data PredicateFailure POOL = StakePoolNotRegisteredOnKeyPOOL
                               | WrongCertificateTypePOOL
                  deriving (Show, Eq)

    initialRules = [ pure emptyDelegation ]
    transitionRules = [ poolDelegationTransition ]

poolDelegationTransition :: TransitionRule POOL
poolDelegationTransition = do
  TRC(slot, d, c) <- judgmentContext
  case c of
    RegPool _      -> pure $ applyDCert slot c d
    RetirePool _ _ -> do
           validStakePoolRetire c d == Valid ?! StakePoolNotRegisteredOnKeyPOOL
           pure $ applyDCert slot c d
    _   -> do
           False ?! WrongCertificateTypePOOL
           pure $ applyDCert slot c d

data DELPL
instance STS DELPL where
    type State DELPL       = DelegationState
    type Signal DELPL      = DCert
    type Environment DELPL = Slot
    data PredicateFailure DELPL = PoolFailure (PredicateFailure POOL)
                                | DelegFailure (PredicateFailure DELEG)
                   deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delplTransition      ]

delplTransition :: TransitionRule DELPL
delplTransition = do
  TRC(slot, d, c) <- judgmentContext
  case c of
    RegPool    _   -> trans @POOL  $ TRC (slot, d, c)
    RetirePool _ _ -> trans @POOL  $ TRC (slot, d, c)
    RegKey _       -> trans @DELEG $ TRC (slot, d, c)
    DeRegKey _     -> trans @DELEG $ TRC (slot, d, c)
    Delegate _     -> trans @DELEG $ TRC (slot, d, c)

instance Embed POOL DELPL where
    wrapFailed = PoolFailure

instance Embed DELEG DELPL where
    wrapFailed = DelegFailure


data DELEGS
instance STS DELEGS where
    type State DELEGS       = DelegationState
    type Signal DELEGS      = [DCert]
    type Environment DELEGS = Slot
    data PredicateFailure DELEGS = DelplFailure (PredicateFailure DELPL)
                    deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delegsTransition     ]

delegsTransition :: TransitionRule DELEGS
delegsTransition = do
  TRC(slot, d, certificates) <- judgmentContext
  foldM (\d' c -> trans @DELPL $ TRC(slot, d', c)) d certificates

instance Embed DELPL DELEGS where
    wrapFailed = DelplFailure

data DELEGT
instance STS DELEGT where
    type State DELEGT       = DelegationState
    type Signal DELEGT      = Tx
    type Environment DELEGT = Slot
    data PredicateFailure DELEGT = DelegsFailure (PredicateFailure DELEGS)
                                 | DelrwdsFailure (PredicateFailure DELRWDS)
                    deriving (Show, Eq)

    initialRules    = [ pure emptyDelegation ]
    transitionRules = [ delegtTransition     ]

delegtTransition :: TransitionRule DELEGT
delegtTransition = do
  TRC(slot, d, tx) <- judgmentContext
  d'  <- trans @DELRWDS $ TRC(slot, d, tx ^. wdrls)
  d'' <- trans @DELEGS $ TRC(slot, d', tx ^. certs)
  pure d''

instance Embed DELRWDS DELEGT where
    wrapFailed = DelrwdsFailure

instance Embed DELEGS DELEGT where
    wrapFailed = DelegsFailure

data LEDGER

instance STS LEDGER where
    type State LEDGER       = (UTxOState, DelegationState)
    type Signal LEDGER      = TxWits
    type Environment LEDGER = (PrtlConsts, Slot)
    data PredicateFailure LEDGER = UtxowFailure (PredicateFailure UTXOW)
                    deriving (Show, Eq)

    initialRules    = [ initialLedgerStateLEDGER ]
    transitionRules = [ ledgerTransition ]

initialLedgerStateLEDGER :: InitialRule LEDGER
initialLedgerStateLEDGER = do
  IRC ((pc, slot)) <- judgmentContext
  utxo' <- trans @UTXOW $ IRC ((pc, slot, Map.empty, Map.empty))
  pure (utxo', emptyDelegation)

ledgerTransition :: TransitionRule LEDGER
ledgerTransition = do
  TRC ((pc, slot), (u, d), txwits) <- judgmentContext
  utxo' <- trans @UTXOW $ TRC ((pc, slot, d ^. stPools, d ^. stKeys), u, txwits)
  pure (utxo', emptyDelegation)

instance Embed UTXOW LEDGER where
    wrapFailed = UtxowFailure
