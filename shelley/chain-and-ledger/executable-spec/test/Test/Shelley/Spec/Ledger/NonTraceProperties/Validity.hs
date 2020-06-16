{-# LANGUAGE Rank2Types #-}

module Test.Shelley.Spec.Ledger.NonTraceProperties.Validity where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Core (dom)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.Certificates (StakePools (..))
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    Hash,
  )
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    DState (..),
    LedgerState (..),
    PState (..),
    RewardAccounts,
    UTxOState (..),
    consumed,
    diffWitHashes,
    minfee,
    nullWitHashes,
    produced,
    verifiedWits,
    witsFromWitnessSet,
    witsVKeyNeeded,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams,
  )
import Shelley.Spec.Ledger.Slot
  ( SlotNo (..),
  )
import Shelley.Spec.Ledger.Tx (Tx (..))
import Shelley.Spec.Ledger.TxData
  ( TxBody (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (txins)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)

-- | Validation errors represent the failures of a transaction to be valid
--  for a given ledger state.
data ValidationError
  = -- | The transaction inputs are not valid.
    BadInputs
  | -- | The transaction has expired
    Expired SlotNo SlotNo
  | -- | Pool Retirement Certificate expired
    RetirementCertExpired SlotNo SlotNo
  | -- | The transaction fee is too small
    FeeTooSmall Coin Coin
  | -- | Value is not conserved
    ValueNotConserved Coin Coin
  | -- | Unknown reward account
    IncorrectRewards
  | -- | One of the transaction witnesses is invalid.
    InvalidWitness
  | -- | The transaction does not have the required witnesses.
    MissingWitnesses
  | -- | Missing Replay Attack Protection, at least one input must be spent.
    InputSetEmpty
  | -- | A stake key cannot be registered again.
    StakeKeyAlreadyRegistered
  | -- | A stake key must be registered to be used or deregistered.
    StakeKeyNotRegistered
  | -- | The stake key to which is delegated is not known.
    StakeDelegationImpossible
  | -- | Stake pool not registered for key, cannot be retired.
    StakePoolNotRegisteredOnKey
  | -- | Other error (for conversion from STS errors that have no correspondance here)
    UnknownValidationError
  deriving (Show, Eq)

-- | The validity of a transaction, where an invalid transaction
--  is represented by list of errors.
data Validity = Valid | Invalid [ValidationError] deriving (Show, Eq)

instance Semigroup Validity where
  Valid <> b = b
  a <> Valid = a
  (Invalid a) <> (Invalid b) = Invalid (a ++ b)

instance Monoid Validity where
  mempty = Valid
  mappend = (<>)

-- | A ledger validation state consists of a ledger state 't' and the list of
-- validation errors that occurred from a valid 's' to reach 't'.
data LedgerValidation h
  = LedgerValidation [ValidationError] (LedgerState (ConcreteCrypto h))
  deriving (Show, Eq)

-- | Determine if the inputs in a transaction are valid for a given ledger state.
validInputs ::
  Crypto crypto =>
  TxBody crypto ->
  UTxOState crypto ->
  Validity
validInputs tx u =
  if txins tx `Set.isSubsetOf` dom (_utxo u)
    then Valid
    else Invalid [BadInputs]

-- | Determine if the transaction has expired
current :: Crypto crypto => TxBody crypto -> SlotNo -> Validity
current tx slot =
  if _ttl tx < slot
    then Invalid [Expired (_ttl tx) slot]
    else Valid

-- | Determine if the input set of a transaction consumes at least one input,
-- else it would be possible to do a replay attack using this transaction.
validNoReplay :: Crypto crypto => TxBody crypto -> Validity
validNoReplay tx =
  if txins tx == Set.empty
    then Invalid [InputSetEmpty]
    else Valid

-- | Determine if the fee is large enough
validFee :: forall crypto. (Crypto crypto) => PParams -> Tx crypto -> Validity
validFee pc tx =
  if needed <= given
    then Valid
    else Invalid [FeeTooSmall needed given]
  where
    needed = minfee pc tx
    given = (_txfee . _body) tx

-- | Determine if the balance of the ledger state would be effected
--  in an acceptable way by a transaction.
preserveBalance ::
  (Crypto crypto) =>
  StakePools crypto ->
  PParams ->
  TxBody crypto ->
  UTxOState crypto ->
  Validity
preserveBalance stakePools pp tx u =
  if destroyed' == created'
    then Valid
    else Invalid [ValueNotConserved destroyed' created']
  where
    destroyed' = consumed pp (_utxo u) tx
    created' = produced pp stakePools tx

-- | Determine if the reward witdrawals correspond
--  to the rewards in the ledger state
correctWithdrawals ::
  RewardAccounts crypto ->
  RewardAccounts crypto ->
  Validity
correctWithdrawals accs withdrawals =
  if withdrawals `Map.isSubmapOf` accs
    then Valid
    else Invalid [IncorrectRewards]

validRuleUTXO ::
  (Crypto crypto) =>
  RewardAccounts crypto ->
  StakePools crypto ->
  PParams ->
  SlotNo ->
  Tx crypto ->
  UTxOState crypto ->
  Validity
validRuleUTXO accs stakePools pc slot tx u =
  validInputs txb u
    <> current txb slot
    <> validNoReplay txb
    <> validFee pc tx
    <> preserveBalance stakePools pc txb u
    <> correctWithdrawals accs (unWdrl $ _wdrls txb)
  where
    txb = _body tx

-- | Given a ledger state, determine if the UTxO witnesses in a given
--  transaction are correct.
verifiedWits' ::
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Tx crypto ->
  Validity
verifiedWits' tx =
  case verifiedWits tx of
    (Right ()) -> Valid
    (Left _failures) -> Invalid [InvalidWitness]

-- | Given a ledger state, determine if the UTxO witnesses in a given
--  transaction are sufficient.
--  We check that there are not more witnesses than inputs, if several inputs
--  from the same address are used, it is not strictly necessary to include more
--  than one witness.
enoughWits ::
  Crypto crypto =>
  Tx crypto ->
  GenDelegs crypto ->
  UTxOState crypto ->
  Validity
enoughWits tx@(Tx _ wits _) d' u =
  if nullWitHashes $ witsVKeyNeeded (_utxo u) tx d' `diffWitHashes` signers
    then Valid
    else Invalid [MissingWitnesses]
  where
    signers = witsFromWitnessSet wits

validRuleUTXOW ::
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Tx crypto ->
  GenDelegs crypto ->
  LedgerState crypto ->
  Validity
validRuleUTXOW tx d' l =
  verifiedWits' tx
    <> enoughWits tx d' (_utxoState l)

validTx ::
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Tx crypto ->
  GenDelegs crypto ->
  SlotNo ->
  PParams ->
  LedgerState crypto ->
  Validity
validTx tx d' slot pp l =
  validRuleUTXO
    ((_rewards . _dstate . _delegationState) l)
    ((_stPools . _pstate . _delegationState) l)
    pp
    slot
    tx
    (_utxoState l)
    <> validRuleUTXOW tx d' l
