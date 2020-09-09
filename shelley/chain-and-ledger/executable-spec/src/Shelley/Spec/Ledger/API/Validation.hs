{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Interface to the block validation and chain extension logic in the Shelley
-- API.
module Shelley.Spec.Ledger.API.Validation
  ( ShelleyState,
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
    applyTickTransition,
    applyBlockTransition,
    reapplyBlockTransition,
  )
where

import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
  ( TRC (..),
    applySTS,
    reapplySTS,
  )
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Keys
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.PParams (PParams)
import qualified Shelley.Spec.Ledger.STS.Bbody as STS
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Tick as STS
import Shelley.Spec.Ledger.Slot (SlotNo)
import qualified Shelley.Spec.Ledger.TxBody as Tx

-- | Type alias for the state updated by TICK and BBODY rules
type ShelleyState = LedgerState.NewEpochState

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}
chainChecks ::
  forall era m.
  (Era era, MonadError (STS.PredicateFailure (STS.CHAIN era)) m) =>
  Globals ->
  PParams ->
  BHeader era ->
  m ()
chainChecks globals pp bh = STS.chainChecks (maxMajorPV globals) pp bh

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkTickEnv ::
  ShelleyState era ->
  STS.TickEnv era
mkTickEnv = STS.TickEnv . LedgerState.getGKeys

mkBbodyEnv ::
  ShelleyState era ->
  STS.BbodyEnv era
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesOsched,
      LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodySlots = nesOsched,
        STS.bbodyPp = LedgerState.esPp nesEs,
        STS.bbodyAccount = LedgerState.esAccountState nesEs
      }

newtype TickTransitionError era
  = TickTransitionError [STS.PredicateFailure (STS.TICK era)]
  deriving (Generic)

instance
  (NoUnexpectedThunks (STS.PredicateFailure (STS.TICK era))) =>
  NoUnexpectedThunks (TickTransitionError era)

deriving stock instance
  (Eq (STS.PredicateFailure (STS.TICK era))) =>
  Eq (TickTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (STS.TICK era))) =>
  Show (TickTransitionError era)

-- | Apply the header level ledger transition.
--
-- This handles checks and updates that happen on a slot tick, as well as a few
-- header level checks, such as size constraints.
applyTickTransition ::
  forall era c.
  (Era era, era ~ Shelley c) =>
  Globals ->
  ShelleyState era ->
  SlotNo ->
  ShelleyState era
applyTickTransition globals state hdr =
  (either err id) . flip runReader globals
    . applySTS @(STS.TICK era)
    $ TRC (mkTickEnv state, state, hdr)
  where
    err :: Show a => a -> b
    err msg = error $ "Panic! applyHeaderTransition failed: " <> (show msg)

newtype BlockTransitionError era
  = BlockTransitionError [STS.PredicateFailure (STS.BBODY era)]
  deriving (Generic)

deriving stock instance
  (Eq (STS.PredicateFailure (STS.BBODY era))) =>
  Eq (BlockTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (STS.BBODY era))) =>
  Show (BlockTransitionError era)

instance
  (NoUnexpectedThunks (STS.PredicateFailure (STS.BBODY era))) =>
  NoUnexpectedThunks (BlockTransitionError era)

-- | Apply the block level ledger transition.
applyBlockTransition ::
  forall era m c.
  ( Era era,
    era ~ Shelley c,
    MonadError (BlockTransitionError era) m,
    DSignable era (Hash era (Tx.TxBody era))
  ) =>
  Globals ->
  ShelleyState era ->
  Block era ->
  m (ShelleyState era)
applyBlockTransition globals state blk =
  liftEither
    . right (updateShelleyState state)
    . left (BlockTransitionError . join)
    $ res
  where
    res =
      flip runReader globals . applySTS @(STS.BBODY era) $
        TRC (mkBbodyEnv state, bbs, blk)
    updateShelleyState ::
      ShelleyState era ->
      STS.BbodyState era ->
      ShelleyState era
    updateShelleyState ss (STS.BbodyState ls bcur) =
      LedgerState.updateNES ss bcur ls
    bbs =
      STS.BbodyState
        (LedgerState.esLState $ LedgerState.nesEs state)
        (LedgerState.nesBcur state)

-- | Re-apply a ledger block to the same state it has been applied to before.
--
--   This function does no validation of whether the block applies successfully;
--   the caller implicitly guarantees that they have previously called
--   `applyBlockTransition` on the same block and that this was successful.
reapplyBlockTransition ::
  forall era c.
  ( Era era,
    era ~ Shelley c,
    DSignable era (Hash era (Tx.TxBody era))
  ) =>
  Globals ->
  ShelleyState era ->
  Block era ->
  ShelleyState era
reapplyBlockTransition globals state blk =
  updateShelleyState state res
  where
    res =
      flip runReader globals . reapplySTS @(STS.BBODY era) $
        TRC (mkBbodyEnv state, bbs, blk)
    updateShelleyState ::
      ShelleyState era ->
      STS.BbodyState era ->
      ShelleyState era
    updateShelleyState ss (STS.BbodyState ls bcur) =
      LedgerState.updateNES ss bcur ls
    bbs =
      STS.BbodyState
        (LedgerState.esLState $ LedgerState.nesEs state)
        (LedgerState.nesBcur state)
