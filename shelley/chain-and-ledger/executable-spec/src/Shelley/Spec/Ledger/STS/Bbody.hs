{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Bbody
  ( BBODY,
    BbodyState (..),
    BbodyEnv (..),
    BbodyPredicateFailure (..),
    PredicateFailure,
    State,
  )
where

import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.Shelley (ShelleyBased)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
    (?!),
  )
import qualified Data.Sequence.Strict as StrictSeq
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (ShelleyBase, epochInfo)
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
    issuerIDfromBHBody,
  )
import Shelley.Spec.Ledger.EpochBoundary (BlocksMade)
import Shelley.Spec.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Shelley.Spec.Ledger.LedgerState
  ( AccountState,
    LedgerState,
  )
import Shelley.Spec.Ledger.OverlaySchedule (isOverlaySlot)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.STS.Ledgers (LEDGERS, LedgersEnv (..))
import Shelley.Spec.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
import Shelley.Spec.Ledger.TxBody (EraIndependentTxBody)

data BBODY era

data BbodyState era
  = BbodyState (LedgerState era) (BlocksMade era)

deriving stock instance
  ShelleyBased era =>
  Show (BbodyState era)

data BbodyEnv era = BbodyEnv
  { bbodyPp :: PParams era,
    bbodyAccount :: AccountState
  }

data BbodyPredicateFailure era
  = WrongBlockBodySizeBBODY
      !Int -- Actual Body Size
      !Int -- Claimed Body Size in Header
  | InvalidBodyHashBBODY
      !(HashBBody (Crypto era)) -- Actual Hash
      !(HashBBody (Crypto era)) -- Claimed Hash
  | LedgersFailure (PredicateFailure (LEDGERS era)) -- Subtransition Failures
  deriving (Generic)

deriving stock instance
  ( ShelleyBased era,
    Show (PredicateFailure (LEDGERS era))
  ) =>
  Show (BbodyPredicateFailure era)

deriving stock instance
  ( ShelleyBased era,
    Eq (PredicateFailure (LEDGERS era))
  ) =>
  Eq (BbodyPredicateFailure era)

instance
  ( ShelleyBased era,
    NoThunks (PredicateFailure (LEDGERS era))
  ) =>
  NoThunks (BbodyPredicateFailure era)

instance
  ( Era era,
    ShelleyBased era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Embed (LEDGERS era) (BBODY era)
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

  type PredicateFailure (BBODY era) = BbodyPredicateFailure era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( ShelleyBased era,
    Embed (LEDGERS era) (BBODY era),
    STS (BBODY era)
  ) =>
  TransitionRule (BBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account,
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
        let hkAsStakePool = coerceKeyRole . issuerIDfromBHBody $ bhb
            slot = bheaderSlotNo bhb
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfo
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e
        pure $
          BbodyState
            ls'
            ( incrBlocks
                (isOverlaySlot firstSlotNo (_d pp) slot)
                hkAsStakePool
                b
            )

instance
  ( Era era,
    STS (LEDGERS era),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    ShelleyBased era
  ) =>
  Embed (LEDGERS era) (BBODY era)
  where
  wrapFailed = LedgersFailure
