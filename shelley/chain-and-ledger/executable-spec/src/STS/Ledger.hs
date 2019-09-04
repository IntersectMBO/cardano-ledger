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
  )
where

import           Lens.Micro ((^.))

import           Keys
import           LedgerState
import           PParams hiding (d)
import           Slot
import           Tx

import           Control.State.Transition

import           STS.Delegs
import           STS.Utxo (UtxoEnv (..))
import           STS.Utxow

data LEDGER hashAlgo dsignAlgo

data LedgerEnv
  = LedgerEnv Slot Ix PParams

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => STS (LEDGER hashAlgo dsignAlgo)
 where
  type State (LEDGER hashAlgo dsignAlgo)
    = (UTxOState hashAlgo dsignAlgo, DPState hashAlgo dsignAlgo)
  type Signal (LEDGER hashAlgo dsignAlgo) = Tx hashAlgo dsignAlgo
  type Environment (LEDGER hashAlgo dsignAlgo) = LedgerEnv
  data PredicateFailure (LEDGER hashAlgo dsignAlgo)
    = UtxowFailure (PredicateFailure (UTXOW hashAlgo dsignAlgo))
    | DelegsFailure (PredicateFailure (DELEGS hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules    = []
  transitionRules = [ledgerTransition]

ledgerTransition
  :: forall hashAlgo dsignAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TransitionRule (LEDGER hashAlgo dsignAlgo)
ledgerTransition = do
  TRC (LedgerEnv slot ix pp, (u, d), tx) <- judgmentContext
  utxo' <- trans @(UTXOW hashAlgo dsignAlgo) $ TRC
    ( UtxoEnv slot pp (d ^. dstate . stKeys) (d ^. pstate . stPools) (d ^. dstate . dms)
    , u
    , tx
    )
  deleg' <-
    trans @(DELEGS hashAlgo dsignAlgo)
      $ TRC (DelegsEnv slot ix pp tx, d, tx ^. body . certs)
  pure (utxo', deleg')

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (DELEGS hashAlgo dsignAlgo) (LEDGER hashAlgo dsignAlgo)
 where
  wrapFailed = DelegsFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (UTXOW hashAlgo dsignAlgo) (LEDGER hashAlgo dsignAlgo)
 where
  wrapFailed = UtxowFailure
