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
  , PredicateFailure (..)
  , State
  )
where

import           Data.Set (Set)

import           Control.State.Transition

import           BlockChain
import           Coin (Coin)
import           EpochBoundary
import           Keys
import           Ledger.Core ((∈))
import           LedgerState
import           PParams
import           Slot
import           STS.Ledgers
import           Tx

data BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo

data BbodyState hashAlgo dsignAlgo vrfAlgo
  = BbodyState (LedgerState hashAlgo dsignAlgo vrfAlgo) (BlocksMade hashAlgo dsignAlgo)
  deriving (Eq, Show)

data BbodyEnv
  = BbodyEnv
    { bbodySlots    :: (Set Slot)
    , bbodyPp       :: PParams
    , bbodyReserves :: Coin
    }

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  )
  => STS (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  type State (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = BbodyState hashAlgo dsignAlgo vrfAlgo

  type Signal (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = Block hashAlgo dsignAlgo kesAlgo vrfAlgo

  type Environment (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo) = BbodyEnv

  data PredicateFailure (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = WrongBlockBodySizeBBODY
    | InvalidBodyHashBBODY
    | LedgersFailure (PredicateFailure (LEDGERS hashAlgo dsignAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition
  :: forall hashAlgo dsignAlgo kesAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     , VRFAlgorithm vrfAlgo
     )
  => TransitionRule (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
bbodyTransition = do
  TRC ( BbodyEnv oslots pp _reserves
      , BbodyState ls b
      , Block (BHeader bhb _) txsSeq@(TxSeq txs)) <- judgmentContext
  let hk = hashKey $ bvkcold bhb

  bBodySize txsSeq == fromIntegral (hBbsize bhb) ?! WrongBlockBodySizeBBODY

  bhbHash txsSeq == bhash bhb ?! InvalidBodyHashBBODY

  ls' <- trans @(LEDGERS hashAlgo dsignAlgo vrfAlgo)
         $ TRC (LedgersEnv (bheaderSlot bhb) pp _reserves, ls, txs)

  pure $ BbodyState ls' (incrBlocks (bheaderSlot bhb ∈ oslots) hk b)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  )
  => Embed (LEDGERS hashAlgo dsignAlgo vrfAlgo) (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = LedgersFailure
