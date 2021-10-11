{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Bbody
  ( BBODY,
    BbodyState (..),
    BbodyEnv (..),
    BbodyPredicateFailure (..),
    BbodyEvent (..),
    PredicateFailure,
    State,
  )
where

import Cardano.Ledger.BHeaderView (BHeaderView (..), isOverlaySlot)
import Cardano.Ledger.BaseTypes (BlocksMade, ShelleyBase, UnitInterval, epochInfo)
import Cardano.Ledger.Block (Block (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era (Crypto), SupportsSegWit (fromTxSeq, hashTxSeq))
import qualified Cardano.Ledger.Era as Era
import Cardano.Ledger.Hashes (EraIndependentBlockBody)
import Cardano.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Cardano.Ledger.Serialization (ToCBORGroup)
import Cardano.Ledger.Shelley.BlockChain (bBodySize, incrBlocks)
import Cardano.Ledger.Shelley.Constraints (UsesAuxiliary, UsesTxBody)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    LedgerState,
    TransLedgerState,
  )
import Cardano.Ledger.Shelley.Rules.Ledgers (LedgersEnv (..))
import Cardano.Ledger.Shelley.TxBody (EraIndependentTxBody)
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
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
import GHC.Records
import NoThunks.Class (NoThunks (..))

data BBODY era

data BbodyState era
  = BbodyState (LedgerState era) (BlocksMade (Crypto era))

deriving stock instance
  TransLedgerState Show era =>
  Show (BbodyState era)

deriving stock instance
  TransLedgerState Eq era =>
  Eq (BbodyState era)

data BbodyEnv era = BbodyEnv
  { bbodyPp :: Core.PParams era,
    bbodyAccount :: AccountState
  }

data BbodyPredicateFailure era
  = WrongBlockBodySizeBBODY
      !Int -- Actual Body Size
      !Int -- Claimed Body Size in Header
  | InvalidBodyHashBBODY
      !(Hash (Crypto era) EraIndependentBlockBody) -- Actual Hash
      !(Hash (Crypto era) EraIndependentBlockBody) -- Claimed Hash
  | LedgersFailure (PredicateFailure (Core.EraRule "LEDGERS" era)) -- Subtransition Failures
  deriving (Generic)

newtype BbodyEvent era
  = LedgersEvent (Event (Core.EraRule "LEDGERS" era))

deriving stock instance
  ( Era era,
    Show (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) =>
  Show (BbodyPredicateFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) =>
  Eq (BbodyPredicateFailure era)

instance
  ( Era era,
    NoThunks (PredicateFailure (Core.EraRule "LEDGERS" era))
  ) =>
  NoThunks (BbodyPredicateFailure era)

instance
  ( UsesTxBody era,
    UsesAuxiliary era,
    ToCBORGroup (Era.TxSeq era),
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Embed (Core.EraRule "LEDGERS" era) (BBODY era),
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Core.Tx era),
    HasField "_d" (Core.PParams era) UnitInterval
  ) =>
  STS (BBODY era)
  where
  type
    State (BBODY era) =
      BbodyState era

  type
    Signal (BBODY era) =
      Block BHeaderView era

  type Environment (BBODY era) = BbodyEnv era

  type BaseM (BBODY era) = ShelleyBase

  type PredicateFailure (BBODY era) = BbodyPredicateFailure era

  type Event (BBODY era) = BbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( STS (BBODY era),
    UsesTxBody era,
    ToCBORGroup (Era.TxSeq era),
    Embed (Core.EraRule "LEDGERS" era) (BBODY era),
    Environment (Core.EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (Core.EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (Core.EraRule "LEDGERS" era) ~ Seq (Core.Tx era),
    HasField "_d" (Core.PParams era) UnitInterval
  ) =>
  TransitionRule (BBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account,
               BbodyState ls b,
               UnserialisedBlock bhview txsSeq
               )
           ) -> do
        let txs = fromTxSeq @era txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = hashTxSeq @era txsSeq

        actualBodySize == fromIntegral (bhviewBSize bhview)
          ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bhview)

        actualBodyHash == bhviewBHash bhview ?! InvalidBodyHashBBODY actualBodyHash (bhviewBHash bhview)

        ls' <-
          trans @(Core.EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bhviewSlot bhview) pp account, ls, StrictSeq.fromStrict txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole . bhviewID $ bhview
            slot = bhviewSlot bhview
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfo
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e
        pure $
          BbodyState
            ls'
            ( incrBlocks
                (isOverlaySlot firstSlotNo (getField @"_d" pp) slot)
                hkAsStakePool
                b
            )

instance
  forall era ledgers.
  ( Era era,
    BaseM ledgers ~ ShelleyBase,
    ledgers ~ Core.EraRule "LEDGERS" era,
    STS ledgers,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era
  ) =>
  Embed ledgers (BBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = LedgersEvent
