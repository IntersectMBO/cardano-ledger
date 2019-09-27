{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Bbody
  ( BBODY
  , BbodyState (..)
  , BbodyEnv (..)
  )
where

import           Data.Set (Set)

import           BlockChain
import           Control.State.Transition
import           EpochBoundary
import           Keys
import           Ledger.Core ((∈))
import           LedgerState
import           PParams
import           Slot
import           STS.Ledgers
import           Tx

data BBODY hashAlgo dsignAlgo kesAlgo

data BbodyState hashAlgo dsignAlgo
  = BbodyState (LedgerState hashAlgo dsignAlgo) (BlocksMade hashAlgo dsignAlgo)

data BbodyEnv
  = BbodyEnv (Set Slot) PParams

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => STS (BBODY hashAlgo dsignAlgo kesAlgo)
 where
  type State (BBODY hashAlgo dsignAlgo kesAlgo) = BbodyState hashAlgo dsignAlgo

  type Signal (BBODY hashAlgo dsignAlgo kesAlgo)
    = Block hashAlgo dsignAlgo kesAlgo

  type Environment (BBODY hashAlgo dsignAlgo kesAlgo) = BbodyEnv

  data PredicateFailure (BBODY hashAlgo dsignAlgo kesAlgo)
    = WrongBlockBodySizeBBODY
    | InvalidBodyHashBBODY
    | LedgersFailure (PredicateFailure (LEDGERS hashAlgo dsignAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition
  :: forall hashAlgo dsignAlgo kesAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     )
  => TransitionRule (BBODY hashAlgo dsignAlgo kesAlgo)
bbodyTransition = do
  TRC ( BbodyEnv oslots pp
      , BbodyState ls b
      , Block (BHeader bhb _) txsSeq@(TxSeq txs)) <- judgmentContext
  let hk = hashKey $ bvkcold bhb

  bBodySize txsSeq == fromIntegral (hBbsize bhb) ?! WrongBlockBodySizeBBODY

  bhbHash txsSeq == bhash bhb ?! InvalidBodyHashBBODY

  ls' <- trans @(LEDGERS hashAlgo dsignAlgo)
         $ TRC (LedgersEnv (bheaderSlot bhb) pp, ls, txs)

  pure $ BbodyState ls' (incrBlocks (bheaderSlot bhb ∈ oslots) hk b)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  )
  => Embed (LEDGERS hashAlgo dsignAlgo) (BBODY hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = LedgersFailure
