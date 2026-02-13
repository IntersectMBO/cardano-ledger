{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
  BbodySignal (..),
  ShelleyBbodyPredFailure (..),
  ShelleyBbodyEvent (..),
  PredicateFailure,
  State,
  validateBodySize,
  validateBodyHash,
) where

import Cardano.Ledger.BaseTypes (
  BlocksMade,
  Mismatch (..),
  ProtVer,
  Relation (..),
  ShelleyBase,
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
import Cardano.Ledger.Block (BbodySignal (..), Block (..), EraBlockHeader (..))
import Cardano.Ledger.Core
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
import Cardano.Ledger.Slot (slotToEpochBoundary)
import Control.DeepSeq (NFData)
import Control.State.Transition (
  Embed (..),
  Rule,
  RuleType (..),
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

deriving instance
  Eq (Event (EraRule "LEDGERS" era)) =>
  Eq (ShelleyBbodyEvent era)

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
  , EraRule "BBODY" era ~ ShelleyBBODY era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  , Embed (EraRule "LEDGERS" era) (ShelleyBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  ) =>
  STS (ShelleyBBODY era)
  where
  type State (ShelleyBBODY era) = ShelleyBbodyState era

  type Signal (ShelleyBBODY era) = BbodySignal era

  type Environment (ShelleyBBODY era) = BbodyEnv era

  type BaseM (ShelleyBBODY era) = ShelleyBase

  type PredicateFailure (ShelleyBBODY era) = ShelleyBbodyPredFailure era

  type Event (ShelleyBBODY era) = ShelleyBbodyEvent era

  initialRules = []
  transitionRules = [bbodyTransition]

bbodyTransition ::
  forall era.
  ( STS (ShelleyBBODY era)
  , EraRule "BBODY" era ~ ShelleyBBODY era
  , EraBlockBody era
  , Embed (EraRule "LEDGERS" era) (ShelleyBBODY era)
  , Environment (EraRule "LEDGERS" era) ~ ShelleyLedgersEnv era
  , Signal (EraRule "LEDGERS" era) ~ Seq (Tx TopTx era)
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  ) =>
  TransitionRule (ShelleyBBODY era)
bbodyTransition = do
  TRC (BbodyEnv pp account, BbodyState ls blocksMade, BbodySignal blk@Block {blockBody}) <-
    judgmentContext

  validateBodySize blk (pp ^. ppProtocolVersionL)

  validateBodyHash blk

  let bhSlot = blk ^. blockHeaderSlotL

  (firstSlot, curEpoch) <- liftSTS $ slotToEpochBoundary bhSlot

  ls' <-
    trans @(EraRule "LEDGERS" era) $
      TRC
        ( LedgersEnv bhSlot curEpoch pp account
        , ls
        , StrictSeq.fromStrict $ blockBody ^. txSeqBlockBodyL
        )

  pure $ BbodyState ls' $ incrBlocks blk firstSlot (pp ^. ppDG) blocksMade

-- | Validate that actual block body size matches claimed size in block header.
validateBodySize ::
  forall h era.
  ( EraBlockHeader h era
  , EraBlockBody era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  ) =>
  Block h era ->
  ProtVer ->
  Rule (EraRule "BBODY" era) 'Transition ()
validateBodySize block protVer =
  actualSize
    == sizeInBlockHeader
      ?! injectFailure
        ( WrongBlockBodySizeBBODY $
            Mismatch
              { mismatchSupplied = actualSize
              , mismatchExpected = sizeInBlockHeader
              }
        )
  where
    actualSize = bBodySize protVer $ blockBody block
    sizeInBlockHeader = fromIntegral $ block ^. blockHeaderBSizeL

-- | Validate that actual block body hash matches claimed hash in block header.
validateBodyHash ::
  forall h era.
  ( EraBlockHeader h era
  , EraBlockBody era
  , InjectRuleFailure "BBODY" ShelleyBbodyPredFailure era
  ) =>
  Block h era ->
  Rule (EraRule "BBODY" era) 'Transition ()
validateBodyHash block =
  actualHash
    == hashInBlockHeader
      ?! injectFailure
        ( InvalidBodyHashBBODY $
            Mismatch
              { mismatchSupplied = actualHash
              , mismatchExpected = hashInBlockHeader
              }
        )
  where
    actualHash = hashBlockBody $ blockBody block
    hashInBlockHeader = block ^. blockHeaderBHashL

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
