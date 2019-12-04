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
  )
where

import           Lens.Micro ((^.))
import           BaseTypes
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

data LEDGER crypto

data LedgerEnv
  = LedgerEnv
    { ledgerSlotNo     :: SlotNo
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
  type BaseM (LEDGER crypto) = ShelleyBase
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
