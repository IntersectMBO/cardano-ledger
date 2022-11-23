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
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Bbody
  ( ShelleyBBODY,
    ShelleyBbodyState (..),
    BbodyEnv (..),
    ShelleyBbodyPredFailure (..),
    ShelleyBbodyEvent (..),
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
import Cardano.Ledger.Shelley.Era (ShelleyBBODY)
import Cardano.Ledger.Shelley.LedgerState
  ( AccountState,
    LedgerState,
  )
import Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLedgersEnv (..))
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

data ShelleyBbodyState era
  = BbodyState (LedgerState era) (BlocksMade (EraCrypto era))

deriving stock instance Show (LedgerState era) => Show (ShelleyBbodyState era)

deriving stock instance Eq (LedgerState era) => Eq (ShelleyBbodyState era)

data BbodyEnv era = BbodyEnv
  { bbodyPp :: PParams era,
    bbodyAccount :: AccountState
  }

data ShelleyBbodyPredFailure era
  = WrongBlockBodySizeBBODY
      !Int -- Actual Body Size
      !Int -- Claimed Body Size in Header
  | InvalidBodyHashBBODY
      !(Hash (EraCrypto era) EraIndependentBlockBody) -- Actual Hash
      !(Hash (EraCrypto era) EraIndependentBlockBody) -- Claimed Hash
  | LedgersFailure (PredicateFailure (EraRule "LEDGERS" era)) -- Subtransition Failures
  deriving (Generic)

newtype ShelleyBbodyEvent era
  = LedgersEvent (Event (EraRule "LEDGERS" era))

deriving stock instance
  ( Era era,
    Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (ShelleyBbodyPredFailure era)

deriving stock instance
  ( Era era,
    Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (ShelleyBbodyPredFailure era)

instance
  ( Era era,
    NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (ShelleyBbodyPredFailure era)

instance
  ( EraSegWits era,
    DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody),
    Embed (EraRule "LEDGERS" era) (ShelleyBBODY era),
    Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era,
    State (EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (EraRule "LEDGERS" era) ~ Seq (Tx era),
    HasField "_d" (PParams era) UnitInterval
  ) =>
  STS (ShelleyBBODY era)
  where
  type
    State (ShelleyBBODY era) =
      ShelleyBbodyState era

  type
    Signal (ShelleyBBODY era) =
      Block (BHeaderView (EraCrypto era)) era

  type Environment (ShelleyBBODY era) = BbodyEnv era

  type BaseM (ShelleyBBODY era) = ShelleyBase

  type PredicateFailure (ShelleyBBODY era) = ShelleyBbodyPredFailure era

  type Event (ShelleyBBODY era) = ShelleyBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( STS (ShelleyBBODY era),
    EraSegWits era,
    Embed (EraRule "LEDGERS" era) (ShelleyBBODY era),
    Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era,
    State (EraRule "LEDGERS" era) ~ LedgerState era,
    Signal (EraRule "LEDGERS" era) ~ Seq (Tx era),
    HasField "_d" (PParams era) UnitInterval
  ) =>
  TransitionRule (ShelleyBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account,
               BbodyState ls b,
               UnserialisedBlock bhview txsSeq
               )
           ) -> do
        let txs = fromTxSeq @era txsSeq
            actualBodySize = bBodySize @era (getField @"_protocolVersion" pp) txsSeq
            actualBodyHash = hashTxSeq @era txsSeq

        actualBodySize
          == fromIntegral (bhviewBSize bhview)
          ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ bhviewBSize bhview)

        actualBodyHash
          == bhviewBHash bhview
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
    DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody),
    Era era
  ) =>
  Embed ledgers (ShelleyBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = LedgersEvent
