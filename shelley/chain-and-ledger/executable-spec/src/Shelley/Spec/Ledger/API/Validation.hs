{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Arrow (left, right)
import Control.Iterate.SetAlgebra (dom, eval)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended (TRC (..), applySTS, reapplySTS)
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
import qualified Shelley.Spec.Ledger.TxData as Tx
import qualified Shelley.Spec.Ledger.Value as Value

-- | Type alias for the state updated by TICK and BBODY rules
type ShelleyState = LedgerState.NewEpochState

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}
chainChecks ::
  forall crypto m v.
  (Value.CV crypto v, MonadError (STS.PredicateFailure (STS.CHAIN crypto v)) m) =>
  Globals ->
  PParams ->
  BHeader crypto v ->
  m ()
chainChecks globals pp bh = STS.chainChecks (maxMajorPV globals) pp bh

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkTickEnv ::
  ShelleyState crypto v ->
  STS.TickEnv crypto
mkTickEnv = STS.TickEnv . LedgerState.getGKeys

mkBbodyEnv ::
  ShelleyState crypto v ->
  STS.BbodyEnv
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesOsched,
      LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodySlots = eval (dom nesOsched),
        STS.bbodyPp = LedgerState.esPp nesEs,
        STS.bbodyAccount = LedgerState.esAccountState nesEs
      }

newtype TickTransitionError crypto v
  = TickTransitionError [STS.PredicateFailure (STS.TICK crypto v)]
  deriving (Eq, Show, Generic)

instance NoUnexpectedThunks (TickTransitionError crypto v)

-- | Apply the header level ledger transition.
--
-- This handles checks and updates that happen on a slot tick, as well as a few
-- header level checks, such as size constraints.
applyTickTransition ::
  forall crypto v.
  (Value.CV crypto v) =>
  Globals ->
  ShelleyState crypto v ->
  SlotNo ->
  ShelleyState crypto v
applyTickTransition globals state hdr =
  (either err id) . flip runReader globals
    . applySTS @(STS.TICK crypto v)
    $ TRC (mkTickEnv state, state, hdr)
  where
    err :: Show a => a -> b
    err msg = error $ "Panic! applyHeaderTransition failed: " <> (show msg)

newtype BlockTransitionError crypto v
  = BlockTransitionError [STS.PredicateFailure (STS.BBODY crypto v)]
  deriving (Eq, Generic, Show)

instance (Value.CV crypto v) => NoUnexpectedThunks (BlockTransitionError crypto v)

-- | Apply the block level ledger transition.
applyBlockTransition ::
  forall crypto m v.
  ( Value.CV crypto v,
    MonadError (BlockTransitionError crypto v) m,
    DSignable crypto (Hash crypto (Tx.TxBody crypto v))
  ) =>
  Globals ->
  ShelleyState crypto v ->
  Block crypto v ->
  m (ShelleyState crypto v)
applyBlockTransition globals state blk =
  liftEither
    . right (updateShelleyState state)
    . left (BlockTransitionError . join)
    $ res
  where
    res =
      flip runReader globals . applySTS @(STS.BBODY crypto v) $
        TRC (mkBbodyEnv state, bbs, blk)
    updateShelleyState ::
      ShelleyState crypto v ->
      STS.BbodyState crypto v ->
      ShelleyState crypto v
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
  forall crypto v.
  ( Value.CV crypto v,
    DSignable crypto (Hash crypto (Tx.TxBody crypto v))
  ) =>
  Globals ->
  ShelleyState crypto v ->
  Block crypto v ->
  ShelleyState crypto v
reapplyBlockTransition globals state blk =
  updateShelleyState state res
  where
    res =
      flip runReader globals . reapplySTS @(STS.BBODY crypto v) $
        TRC (mkBbodyEnv state, bbs, blk)
    updateShelleyState ::
      ShelleyState crypto v ->
      STS.BbodyState crypto v ->
      ShelleyState crypto v
    updateShelleyState ss (STS.BbodyState ls bcur) =
      LedgerState.updateNES ss bcur ls
    bbs =
      STS.BbodyState
        (LedgerState.esLState $ LedgerState.nesEs state)
        (LedgerState.nesBcur state)
