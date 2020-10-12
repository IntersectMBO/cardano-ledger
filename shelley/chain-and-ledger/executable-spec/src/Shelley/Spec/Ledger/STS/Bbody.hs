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

import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
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
import Data.Sequence (Seq)
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
import Shelley.Spec.Ledger.Tx (Tx, TxBody)

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
      !(HashBBody era) -- Actual Hash
      !(HashBBody era) -- Claimed Hash
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
  ( Crypto c,
    DSignable (ShelleyEra c) (Hash (ShelleyEra c) (TxBody (ShelleyEra c)))
  ) =>
  STS (BBODY (ShelleyEra c))
  where
  type
    State (BBODY (ShelleyEra c)) =
      BbodyState (ShelleyEra c)

  type
    Signal (BBODY (ShelleyEra c)) =
      Block (ShelleyEra c)

  type Environment (BBODY (ShelleyEra c)) = BbodyEnv (ShelleyEra c)

  type BaseM (BBODY (ShelleyEra c)) = ShelleyBase

  type PredicateFailure (BBODY (ShelleyEra c)) = BbodyPredicateFailure (ShelleyEra c)

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( ShelleyBased era,
    BaseM (BBODY era) ~ ShelleyBase,
    Embed (LEDGERS era) (BBODY era),
    Environment (BBODY era) ~ BbodyEnv era,
    State (BBODY era) ~ BbodyState era,
    Signal (BBODY era) ~ Block era,
    PredicateFailure (BBODY era) ~ BbodyPredicateFailure era,
    Environment (LEDGERS era) ~ LedgersEnv era,
    State (LEDGERS era) ~ LedgerState era,
    Signal (LEDGERS era) ~ Seq (Tx era)
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
  ( Crypto c,
    DSignable (ShelleyEra c) (Hash (ShelleyEra c) (TxBody (ShelleyEra c)))
  ) =>
  Embed (LEDGERS (ShelleyEra c)) (BBODY (ShelleyEra c))
  where
  wrapFailed = LedgersFailure
