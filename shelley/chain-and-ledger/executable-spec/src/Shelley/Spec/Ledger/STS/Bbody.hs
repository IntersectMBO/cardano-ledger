{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Bbody
  ( BBODY,
    BbodyState (..),
    BbodyEnv (..),
    PredicateFailure (..),
    State,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Iterate.SetAlgebra (eval, (∈))
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
    (?!),
  )
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase)
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    BHeader (..),
    Block (..),
    HashBBody,
    TxSeq (..),
    bBodySize,
    bbHash,
    hBbsize,
    incrBlocks,
    poolIDfromBHBody,
  )
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Shelley.Spec.Ledger.LedgerState (AccountState, LedgerState)
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.Tx (TxBody)
import Shelley.Spec.Ledger.Value

data BBODY crypto v

data BbodyState crypto v
  = BbodyState (LedgerState crypto v) (BlocksMade crypto)
  deriving (Eq, Show)

data BbodyEnv = BbodyEnv
  { bbodySlots :: (Set SlotNo),
    bbodyPp :: PParams,
    bbodyAccount :: AccountState
  }

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  STS (BBODY crypto v)
  where
  type
    State (BBODY crypto v) =
      BbodyState crypto v

  type
    Signal (BBODY crypto v) =
      Block crypto v

  type Environment (BBODY crypto v) = BbodyEnv

  type BaseM (BBODY crypto v) = ShelleyBase

  data PredicateFailure (BBODY crypto v)
    = WrongBlockBodySizeBBODY
        !Int -- Actual Body Size
        !Int -- Claimed Body Size in Header
    | InvalidBodyHashBBODY
        !(HashBBody crypto v) -- Actual Hash
        !(HashBBody crypto v) -- Claimed Hash
    | LedgersFailure (PredicateFailure (LEDGERS crypto v)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [bbodyTransition]

instance (CV crypto v) => NoUnexpectedThunks (PredicateFailure (BBODY crypto v))

bbodyTransition ::
  forall crypto v.
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  TransitionRule (BBODY crypto v)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv oslots pp account,
               BbodyState ls b,
               Block (BHeader bhb _) txsSeq
               )
           ) -> do
        let TxSeq txs = txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = bbHash txsSeq

        actualBodySize == fromIntegral (hBbsize bhb)
          ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ hBbsize bhb)

        actualBodyHash == bhash bhb ?! InvalidBodyHashBBODY actualBodyHash (bhash bhb)

        ls' <-
          trans @(LEDGERS crypto v) $
            TRC (LedgersEnv (bheaderSlotNo bhb) pp account, ls, StrictSeq.getSeq txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole . poolIDfromBHBody $ bhb
        pure $ BbodyState ls' (incrBlocks (eval (bheaderSlotNo bhb ∈ oslots)) hkAsStakePool b)

instance
  ( CV crypto v,
    DSignable crypto (Hash crypto (TxBody crypto v))
  ) =>
  Embed (LEDGERS crypto v) (BBODY crypto v)
  where
  wrapFailed = LedgersFailure
