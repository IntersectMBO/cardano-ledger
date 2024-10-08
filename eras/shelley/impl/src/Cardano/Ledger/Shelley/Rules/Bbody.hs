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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Bbody (
  ShelleyBBODY,
  ShelleyBbodyState (..),
  BbodyEnv (..),
  ShelleyBbodyPredFailure (..),
  ShelleyBbodyEvent (..),
  PredicateFailure,
  State,
)
where

import Cardano.Ledger.BHeaderView (BHeaderView (..), isOverlaySlot)
import Cardano.Ledger.BaseTypes (
  BlocksMade,
  Mismatch (..),
  Relation (..),
  ShelleyBase,
  epochInfoPure,
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (DSignable, Hash, coerceKeyRole)
import Cardano.Ledger.Shelley.BlockChain (incrBlocks)
import Cardano.Ledger.Shelley.Era (ShelleyBBODY, ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (AccountState)
import Cardano.Ledger.Shelley.Rules.Deleg (ShelleyDelegPredFailure)
import Cardano.Ledger.Shelley.Rules.Delegs (ShelleyDelegsPredFailure)
import Cardano.Ledger.Shelley.Rules.Delpl (ShelleyDelplPredFailure)
import Cardano.Ledger.Shelley.Rules.Ledger (ShelleyLedgerPredFailure)
import Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLedgersEnv (..), ShelleyLedgersPredFailure)
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure)
import Cardano.Ledger.Shelley.Rules.Ppup (ShelleyPpupPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxo (ShelleyUtxoPredFailure)
import Cardano.Ledger.Shelley.Rules.Utxow (ShelleyUtxowPredFailure)
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (
  Embed (..),
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
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data ShelleyBbodyState era
  = BbodyState !(State (EraRule "LEDGERS" era)) !(BlocksMade (EraCrypto era))

deriving stock instance Show (State (EraRule "LEDGERS" era)) => Show (ShelleyBbodyState era)

deriving stock instance Eq (State (EraRule "LEDGERS" era)) => Eq (ShelleyBbodyState era)

data BbodyEnv era = BbodyEnv
  { bbodyPp :: PParams era
  , bbodyAccount :: AccountState
  }

data ShelleyBbodyPredFailure era
  = WrongBlockBodySizeBBODY (Mismatch 'RelEQ Int)
  | InvalidBodyHashBBODY (Mismatch 'RelEQ (Hash (EraCrypto era) EraIndependentBlockBody))
  | LedgersFailure (PredicateFailure (EraRule "LEDGERS" era)) -- Subtransition Failures
  deriving (Generic)

type instance EraRuleFailure "BBODY" (ShelleyEra c) = ShelleyBbodyPredFailure (ShelleyEra c)

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure (ShelleyEra c)

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure

instance InjectRuleFailure "BBODY" ShelleyLedgerPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPpupPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegsPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelplPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegPredFailure (ShelleyEra c) where
  injectFailure = LedgersFailure . injectFailure

newtype ShelleyBbodyEvent era
  = LedgersEvent (Event (EraRule "LEDGERS" era))

deriving stock instance
  ( Era era
  , Show (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Show (ShelleyBbodyPredFailure era)

deriving stock instance
  ( Era era
  , Eq (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  Eq (ShelleyBbodyPredFailure era)

instance
  ( Era era
  , NoThunks (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  NoThunks (ShelleyBbodyPredFailure era)

instance
  ( EraSegWits era
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Embed (EraRule "LEDGERS" era) (ShelleyBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
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
  ( STS (ShelleyBBODY era)
  , EraSegWits era
  , Embed (EraRule "LEDGERS" era) (ShelleyBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx era)
  ) =>
  TransitionRule (ShelleyBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
            ( BbodyEnv pp account
              , BbodyState ls b
              , UnserialisedBlock bhview txsSeq
              )
          ) -> do
        let txs = fromTxSeq txsSeq
            actualBodySize = bBodySize (pp ^. ppProtocolVersionL) txsSeq
            actualBodyHash = hashTxSeq txsSeq

        actualBodySize
          == fromIntegral (bhviewBSize bhview)
            ?! WrongBlockBodySizeBBODY
              ( Mismatch
                  { mismatchSupplied = actualBodySize
                  , mismatchExpected = fromIntegral $ bhviewBSize bhview
                  }
              )

        actualBodyHash
          == bhviewBHash bhview
            ?! InvalidBodyHashBBODY
              ( Mismatch
                  { mismatchSupplied = actualBodyHash
                  , mismatchExpected = bhviewBHash bhview
                  }
              )

        ls' <-
          trans @(EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bhviewSlot bhview) pp account, ls, StrictSeq.fromStrict txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole $ bhviewID bhview
            slot = bhviewSlot bhview
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfoPure
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e
        let isOverlay = isOverlaySlot firstSlotNo (pp ^. ppDG) slot
        pure $ BbodyState ls' (incrBlocks isOverlay hkAsStakePool b)

instance
  forall era ledgers.
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  , DSignable (EraCrypto era) (Hash (EraCrypto era) EraIndependentTxBody)
  , Era era
  ) =>
  Embed ledgers (ShelleyBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = LedgersEvent
