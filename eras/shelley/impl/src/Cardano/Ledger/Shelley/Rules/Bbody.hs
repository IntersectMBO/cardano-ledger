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
import Cardano.Ledger.BaseTypes (BlocksMade, ShelleyBase, UnitInterval, epochInfoPure)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Cardano.Ledger.Shelley.BlockChain (bBodySize, incrBlocks)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    LedgerState,
  )
import Cardano.Ledger.Shelley.Rules.Ledgers (LedgersEnv (..))
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
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))

data BBODY era

data BbodyState era
  = BbodyState (LedgerState era) (BlocksMade (Crypto era))

deriving stock instance Show (LedgerState era) => Show (BbodyState era)

deriving stock instance Eq (LedgerState era) => Eq (BbodyState era)

data BbodyEnv era = BbodyEnv
  { bbodyPp :: PParams era,
    bbodyAccount :: AccountState
  }

data BbodyPredicateFailure era
  = WrongBlockBodySizeBBODY
      !Int -- Actual Body Size
      !Int -- Claimed Body Size in Header
  | InvalidBodyHashBBODY
      !(Hash (Crypto era) EraIndependentBlockBody) -- Actual Hash
      !(Hash (Crypto era) EraIndependentBlockBody) -- Claimed Hash
  | LedgersFailure (PredicateFailure (EraRule "LEDGERS" era)) -- Subtransition Failures
  deriving (Generic)

newtype BbodyEvent era
  = LedgersEvent (Event (EraRule "LEDGERS" era))

deriving stock instance
  ( Era era,
    Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (BbodyPredicateFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (BbodyPredicateFailure era)

instance
  ( Era era,
    NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (BbodyPredicateFailure era)

instance
  ( EraSegWits era,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Embed (EraRule "LEDGERS" era) (BBODY era),
    Environment (EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (EraRule "LEDGERS" era) ~ Seq (Tx era),
    HasField "_d" (PParams era) UnitInterval
  ) =>
  STS (BBODY era)
  where
  type
    State (BBODY era) =
      BbodyState era

  type
    Signal (BBODY era) =
      Block (BHeaderView (Crypto era)) era

  type Environment (BBODY era) = BbodyEnv era

  type BaseM (BBODY era) = ShelleyBase

  type PredicateFailure (BBODY era) = BbodyPredicateFailure era

  type Event (BBODY era) = BbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( STS (BBODY era),
    EraSegWits era,
    Embed (EraRule "LEDGERS" era) (BBODY era),
    Environment (EraRule "LEDGERS" era) ~ LedgersEnv era,
    State (EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (EraRule "LEDGERS" era) ~ Seq (Tx era),
    HasField "_d" (PParams era) UnitInterval
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

        actualBodyHash == bhviewBHash bhview
          ?! InvalidBodyHashBBODY actualBodyHash (bhviewBHash bhview)

        ls' <-
          trans @(EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bhviewSlot bhview) pp account, ls, StrictSeq.fromStrict txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole . bhviewID $ bhview
            slot = bhviewSlot bhview
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfoPure
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e
        let isOverlay = isOverlaySlot firstSlotNo (getField @"_d" pp) slot
        pure $ BbodyState ls' (incrBlocks isOverlay hkAsStakePool b)

instance
  forall era ledgers.
  ( Era era,
    BaseM ledgers ~ ShelleyBase,
    ledgers ~ EraRule "LEDGERS" era,
    STS ledgers,
    DSignable (Crypto era) (Hash (Crypto era) EraIndependentTxBody),
    Era era
  ) =>
  Embed ledgers (BBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = LedgersEvent
