{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
) where

import Cardano.Ledger.BHeaderView (BHeaderView (..), isOverlaySlot)
import Cardano.Ledger.BaseTypes (
  BlocksMade,
  Mismatch (..),
  Relation (..),
  ShelleyBase,
  epochInfoPure,
 )
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders (
  Decode (From, FromGroup, Invalid, SumD, Summands),
  Encode (Sum, To, ToGroup),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Shelley.BlockBody (incrBlocks)
import Cardano.Ledger.Shelley.Era (ShelleyBBODY, ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (ChainAccountState)
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
import Control.DeepSeq (NFData)
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
  = BbodyState !(State (EraRule "LEDGERS" era)) !BlocksMade
  deriving (Generic)

deriving stock instance Show (State (EraRule "LEDGERS" era)) => Show (ShelleyBbodyState era)

deriving stock instance Eq (State (EraRule "LEDGERS" era)) => Eq (ShelleyBbodyState era)

data BbodyEnv era = BbodyEnv
  { bbodyPp :: PParams era
  , bbodyAccount :: ChainAccountState
  }

data ShelleyBbodyPredFailure era
  = -- | `mismatchSupplied` ~ Actual body size.
    --   `mismatchExpected` ~ Claimed body size in the header.
    WrongBlockBodySizeBBODY (Mismatch RelEQ Int)
  | -- | `mismatchSupplied` ~ Actual hash.
    --   `mismatchExpected` ~ Claimed hash in the header.
    InvalidBodyHashBBODY (Mismatch RelEQ (Hash HASH EraIndependentBlockBody))
  | LedgersFailure (PredicateFailure (EraRule "LEDGERS" era)) -- Subtransition Failures
  deriving (Generic)

instance NFData (PredicateFailure (EraRule "LEDGERS" era)) => NFData (ShelleyBbodyPredFailure era)

type instance EraRuleFailure "BBODY" ShelleyEra = ShelleyBbodyPredFailure ShelleyEra

instance InjectRuleFailure "BBODY" ShelleyBbodyPredFailure ShelleyEra

instance InjectRuleFailure "BBODY" ShelleyLedgersPredFailure ShelleyEra where
  injectFailure = LedgersFailure

instance InjectRuleFailure "BBODY" ShelleyLedgerPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxowPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyUtxoPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPpupPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegsPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelplPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyPoolPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance InjectRuleFailure "BBODY" ShelleyDelegPredFailure ShelleyEra where
  injectFailure = LedgersFailure . injectFailure

instance
  ( Era era
  , EncCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  EncCBOR (ShelleyBbodyPredFailure era)
  where
  encCBOR =
    encode . \case
      WrongBlockBodySizeBBODY mm -> Sum WrongBlockBodySizeBBODY 0 !> ToGroup mm
      InvalidBodyHashBBODY mm -> Sum (InvalidBodyHashBBODY @era) 1 !> ToGroup mm
      LedgersFailure x -> Sum (LedgersFailure @era) 2 !> To x

instance
  ( Era era
  , DecCBOR (PredicateFailure (EraRule "LEDGERS" era))
  ) =>
  DecCBOR (ShelleyBbodyPredFailure era)
  where
  decCBOR = decode . Summands "ShelleyBbodyPredFailure" $ \case
    0 -> SumD WrongBlockBodySizeBBODY <! FromGroup
    1 -> SumD InvalidBodyHashBBODY <! FromGroup
    2 -> SumD LedgersFailure <! From
    n -> Invalid n

newtype ShelleyBbodyEvent era
  = LedgersEvent (Event (EraRule "LEDGERS" era))
  deriving (Generic)

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
  ( EraBlockBody era
  , Embed (EraRule "LEDGERS" era) (ShelleyBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  ) =>
  STS (ShelleyBBODY era)
  where
  type State (ShelleyBBODY era) = ShelleyBbodyState era

  type Signal (ShelleyBBODY era) = Block BHeaderView era

  type Environment (ShelleyBBODY era) = BbodyEnv era

  type BaseM (ShelleyBBODY era) = ShelleyBase

  type PredicateFailure (ShelleyBBODY era) = ShelleyBbodyPredFailure era

  type Event (ShelleyBBODY era) = ShelleyBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( STS (ShelleyBBODY era)
  , EraBlockBody era
  , Embed (EraRule "LEDGERS" era) (ShelleyBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  ) =>
  TransitionRule (ShelleyBBODY era)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv pp account
               , BbodyState ls b
               , Block {blockHeader = blockHeaderView, blockBody}
               )
           ) -> do
        let txs = blockBody ^. txSeqBlockBodyL
            actualBodySize = bBodySize (pp ^. ppProtocolVersionL) blockBody
            actualBodyHash = hashBlockBody blockBody

        actualBodySize
          == fromIntegral (bhviewBSize blockHeaderView)
            ?! WrongBlockBodySizeBBODY
              ( Mismatch
                  { mismatchSupplied = actualBodySize
                  , mismatchExpected = fromIntegral $ bhviewBSize blockHeaderView
                  }
              )

        actualBodyHash
          == bhviewBHash blockHeaderView
            ?! InvalidBodyHashBBODY
              ( Mismatch
                  { mismatchSupplied = actualBodyHash
                  , mismatchExpected = bhviewBHash blockHeaderView
                  }
              )
        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole $ bhviewID blockHeaderView
            slot = bhviewSlot blockHeaderView
        (firstSlotNo, curEpochNo) <- liftSTS $ do
          ei <- asks epochInfoPure
          let curEpochNo = epochInfoEpoch ei slot
          pure (epochInfoFirst ei curEpochNo, curEpochNo)

        ls' <-
          trans @(EraRule "LEDGERS" era) $
            TRC (LedgersEnv (bhviewSlot blockHeaderView) curEpochNo pp account, ls, StrictSeq.fromStrict txs)

        let isOverlay = isOverlaySlot firstSlotNo (pp ^. ppDG) slot
        pure $ BbodyState ls' (incrBlocks isOverlay hkAsStakePool b)

instance
  forall era ledgers.
  ( Era era
  , BaseM ledgers ~ ShelleyBase
  , ledgers ~ EraRule "LEDGERS" era
  , STS ledgers
  , Era era
  ) =>
  Embed ledgers (ShelleyBBODY era)
  where
  wrapFailed = LedgersFailure
  wrapEvent = LedgersEvent
