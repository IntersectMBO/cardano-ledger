{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Coin (Coin)
import           Control.State.Transition
import           Keys
import           LedgerState
import           PParams hiding (d)
import           Slot
import           STS.Delegs
import           STS.Utxo (UtxoEnv (..))
import           STS.Utxow
import           Tx
import           Cardano.Ledger.Shelley.Crypto

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
