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

data LEDGER hashAlgo dsignAlgo vrfAlgo

data LedgerEnv
  = LedgerEnv
    { ledgerSlot     :: Slot
    , ledgerIx       :: Ix
    , ledgerPp       :: PParams
    , ledgerReserves :: Coin
    }
  deriving (Show)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  )
  => STS (LEDGER hashAlgo dsignAlgo vrfAlgo)
 where
  type State (LEDGER hashAlgo dsignAlgo vrfAlgo)
    = (UTxOState hashAlgo dsignAlgo vrfAlgo, DPState hashAlgo dsignAlgo vrfAlgo)
  type Signal (LEDGER hashAlgo dsignAlgo vrfAlgo) = Tx hashAlgo dsignAlgo vrfAlgo
  type Environment (LEDGER hashAlgo dsignAlgo vrfAlgo) = LedgerEnv
  data PredicateFailure (LEDGER hashAlgo dsignAlgo vrfAlgo)
    = UtxowFailure (PredicateFailure (UTXOW hashAlgo dsignAlgo vrfAlgo))
    | DelegsFailure (PredicateFailure (DELEGS hashAlgo dsignAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [ledgerTransition]

ledgerTransition
  :: forall hashAlgo dsignAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     , VRFAlgorithm vrfAlgo
     )
  => TransitionRule (LEDGER hashAlgo dsignAlgo vrfAlgo)
ledgerTransition = do
  TRC (LedgerEnv slot ix pp _reserves, (u, d), tx) <- judgmentContext
  utxo' <- trans @(UTXOW hashAlgo dsignAlgo vrfAlgo) $ TRC
    ( UtxoEnv slot pp (d ^. dstate . stkCreds) (d ^. pstate . stPools) (d ^. dstate . genDelegs)
    , u
    , tx
    )
  deleg' <-
    trans @(DELEGS hashAlgo dsignAlgo vrfAlgo)
      $ TRC (DelegsEnv slot ix pp tx _reserves, d, tx ^. body . certs)
  pure (utxo', deleg')

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  )
  => Embed (DELEGS hashAlgo dsignAlgo vrfAlgo) (LEDGER hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = DelegsFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  )
  => Embed (UTXOW hashAlgo dsignAlgo vrfAlgo) (LEDGER hashAlgo dsignAlgo vrfAlgo)
 where
  wrapFailed = UtxowFailure
