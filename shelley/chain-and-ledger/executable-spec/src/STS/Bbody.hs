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
import           Cardano.Ledger.Shelley.Crypto

data BBODY crypto

data BbodyState crypto
  = BbodyState (LedgerState crypto) (BlocksMade crypto)
  deriving (Eq, Show)

data BbodyEnv
  = BbodyEnv
    { bbodySlots    :: (Set Slot)
    , bbodyPp       :: PParams
    , bbodyReserves :: Coin
    }

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => STS (BBODY crypto)
 where
  type State (BBODY crypto)
    = BbodyState crypto

  type Signal (BBODY crypto)
    = Block crypto

  type Environment (BBODY crypto) = BbodyEnv

  data PredicateFailure (BBODY crypto)
    = WrongBlockBodySizeBBODY
    | InvalidBodyHashBBODY
    | LedgersFailure (PredicateFailure (LEDGERS crypto))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TransitionRule (BBODY crypto)
bbodyTransition = do
  TRC ( BbodyEnv oslots pp _reserves
      , BbodyState ls b
      , Block (BHeader bhb _) txsSeq@(TxSeq txs)) <- judgmentContext
  let hk = hashKey $ bvkcold bhb

  bBodySize txsSeq == fromIntegral (hBbsize bhb) ?! WrongBlockBodySizeBBODY

  bhbHash txsSeq == bhash bhb ?! InvalidBodyHashBBODY

  ls' <- trans @(LEDGERS crypto)
         $ TRC (LedgersEnv (bheaderSlot bhb) pp _reserves, ls, txs)

  pure $ BbodyState ls' (incrBlocks (bheaderSlot bhb ∈ oslots) hk b)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (LEDGERS crypto) (BBODY crypto)
 where
  wrapFailed = LedgersFailure
