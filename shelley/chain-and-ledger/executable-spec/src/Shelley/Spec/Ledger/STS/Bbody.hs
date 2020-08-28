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

import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NoUnexpectedThunks (..))
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
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    LedgerState,
  )
import Shelley.Spec.Ledger.OverlaySchedule
  ( OverlaySchedule,
    isOverlaySlot,
  )
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersEnv (..))
import Shelley.Spec.Ledger.Tx (TxBody)

data BBODY era

data BbodyState era
  = BbodyState (LedgerState era) (BlocksMade era)
  deriving (Eq, Show)

data BbodyEnv era = BbodyEnv
  { bbodySlots :: OverlaySchedule era,
    bbodyPp :: PParams,
    bbodyAccount :: AccountState
  }

instance
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  STS (BBODY era)
  where
  type
    State (BBODY era) =
      BbodyState era

  type
    Signal (BBODY era) =
      Block era

  type Environment (BBODY era) = BbodyEnv era

  type BaseM (BBODY era) = ShelleyBase

  data PredicateFailure (BBODY era)
    = WrongBlockBodySizeBBODY
        !Int -- Actual Body Size
        !Int -- Claimed Body Size in Header
    | InvalidBodyHashBBODY
        !(HashBBody era) -- Actual Hash
        !(HashBBody era) -- Claimed Hash
    | LedgersFailure (PredicateFailure (LEDGERS era)) -- Subtransition Failures
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [bbodyTransition]

instance (Era era) => NoUnexpectedThunks (PredicateFailure (BBODY era))

bbodyTransition ::
  forall era.
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  TransitionRule (BBODY era)
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
          trans @(LEDGERS era) $
            TRC (LedgersEnv (bheaderSlotNo bhb) pp account, ls, StrictSeq.getSeq txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole . poolIDfromBHBody $ bhb
        pure $ BbodyState ls' (incrBlocks (isOverlaySlot (bheaderSlotNo bhb) oslots) hkAsStakePool b)

instance
  ( Era era,
    DSignable era (Hash era (TxBody era))
  ) =>
  Embed (LEDGERS era) (BBODY era)
  where
  wrapFailed = LedgersFailure
