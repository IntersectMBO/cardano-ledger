{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Ledger
  ( LEDGER
  , LedgerEnv (..)
  , PredicateFailure(..)
  , asStateTransition
  , asStateTransition'
  )
where

import           Lens.Micro ((^.))

import           Cardano.Ledger.Shelley.Crypto
import           Coin (Coin)
import           Control.State.Transition
import           Keys
import           LedgerState
import           PParams hiding (d)
import           Slot
import           STS.Delegs
import           STS.Utxo (pattern BadInputsUTxO, pattern ExpiredUTxO, pattern FeeTooSmallUTxO,
                     pattern InputSetEmptyUTxO, pattern MaxTxSizeUTxO, pattern NegativeOutputsUTxO,
                     pattern UpdateFailure, UtxoEnv (..), pattern ValueNotConservedUTxO)
import           STS.Utxow
import           Tx
import           Validation (ValidationError (..))

data LEDGER crypto

data LedgerEnv
  = LedgerEnv
    { ledgerSlot     :: Slot
    , ledgerIx       :: Ix
    , ledgerPp       :: PParams
    , ledgerReserves :: Coin
    }
  deriving (Show)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => STS (LEDGER crypto)
 where
  type State (LEDGER crypto)
    = (UTxOState crypto, DPState crypto)
  type Signal (LEDGER crypto) = Tx crypto
  type Environment (LEDGER crypto) = LedgerEnv
  data PredicateFailure (LEDGER crypto)
    = UtxowFailure (PredicateFailure (UTXOW crypto))
    | DelegsFailure (PredicateFailure (DELEGS crypto))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [ledgerTransition]

ledgerTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TransitionRule (LEDGER crypto)
ledgerTransition = do
  TRC (LedgerEnv slot ix pp _reserves, (u, d), tx) <- judgmentContext
  utxo' <- trans @(UTXOW crypto) $ TRC
    ( UtxoEnv slot pp (d ^. dstate . stkCreds) (d ^. pstate . stPools) (d ^. dstate . genDelegs)
    , u
    , tx
    )
  deleg' <-
    trans @(DELEGS crypto)
      $ TRC (DelegsEnv slot ix pp tx _reserves, d, tx ^. body . certs)
  pure (utxo', deleg')

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (DELEGS crypto) (LEDGER crypto)
 where
  wrapFailed = DelegsFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (UTXOW crypto) (LEDGER crypto)
 where
  wrapFailed = UtxowFailure

-- |In the case where a transaction is valid for a given ledger state,
-- apply the transaction as a state transition function on the ledger state.
-- Otherwise, return a list of validation errors.
asStateTransition
  :: forall crypto .
     ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Slot
  -> PParams
  -> LedgerState crypto
  -> Tx crypto
  -> Coin
  -> Either [ValidationError] (LedgerState crypto)
asStateTransition slot pp ls tx res =
  let next = applySTS @(LEDGER crypto) (TRC ((LedgerEnv slot (_txSlotIx ls) pp res)
                                         , (_utxoState ls, _delegationState ls)
                                         , tx))
  in
  case next of
    Left pfs -> Left $ convertPredicateFailuresToValidationErrors pfs
    Right (u, d)  -> Right $ ls { _utxoState = u
                                , _delegationState = d
                                , _txSlotIx = 1 + _txSlotIx ls
                                }

-- | Apply transition independent of validity, collect validation errors on the
-- way.
asStateTransition'
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => Slot
  -> PParams
  -> LedgerValidation crypto
  -> Tx crypto
  -> Coin
  -> LedgerValidation crypto
asStateTransition' = undefined

convertPredicateFailuresToValidationErrors :: [[PredicateFailure (LEDGER c)]] -> [ValidationError]
convertPredicateFailuresToValidationErrors pfs =
  map predicateFailureToValidationError $ foldr (++) [] pfs

predicateFailureToValidationError :: PredicateFailure (LEDGER c) -> ValidationError

predicateFailureToValidationError (UtxowFailure (MissingVKeyWitnessesUTXOW))
  = MissingWitnesses
predicateFailureToValidationError (UtxowFailure (MissingScriptWitnessesUTXOW))
  = MissingWitnesses

predicateFailureToValidationError (UtxowFailure (InvalidWitnessesUTXOW))
  = InvalidWitness

predicateFailureToValidationError (UtxowFailure (UtxoFailure InputSetEmptyUTxO))
  = InputSetEmpty

predicateFailureToValidationError (UtxowFailure (UtxoFailure (ExpiredUTxO a b)))
  = Expired a b

predicateFailureToValidationError (UtxowFailure (UtxoFailure BadInputsUTxO))
  = BadInputs

predicateFailureToValidationError (UtxowFailure (UtxoFailure (FeeTooSmallUTxO a b)))
  = FeeTooSmall a b

predicateFailureToValidationError (UtxowFailure (UtxoFailure (ValueNotConservedUTxO a b)))
  = ValueNotConserved a b

predicateFailureToValidationError (DelegsFailure DelegateeNotRegisteredDELEG)
  = StakeDelegationImpossible

predicateFailureToValidationError (DelegsFailure WithrawalsNotInRewardsDELEGS)
  = IncorrectRewards

predicateFailureToValidationError _
  = UnknownValidationError
