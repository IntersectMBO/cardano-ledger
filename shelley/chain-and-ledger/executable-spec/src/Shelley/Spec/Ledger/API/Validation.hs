{-# LANGUAGE DefaultSignatures #-}
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
  ( ApplyBlock (..),
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
  )
where

import Cardano.Ledger.Constraints (UsesValue)
import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyEra)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.STS.Bbody as STS
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Tick as STS
import Shelley.Spec.Ledger.Slot (SlotNo)

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}

class
  ( ChainData (Block era),
    AnnotatedData (Block era),
    ChainData (BHeader (Crypto era)),
    AnnotatedData (BHeader (Crypto era)),
    ChainData (NewEpochState era),
    SerialisableData (NewEpochState era),
    ChainData (BlockTransitionError era),
    ChainData (STS.PredicateFailure (STS.CHAIN era))
  ) =>
  ApplyBlock era
  where
  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTick ::
    Globals ->
    NewEpochState era ->
    SlotNo ->
    NewEpochState era
  default applyTick ::
    (UsesValue era) =>
    Globals ->
    NewEpochState era ->
    SlotNo ->
    NewEpochState era
  applyTick globals state hdr =
    (either err id) . flip runReader globals
      . applySTS @(STS.TICK era)
      $ TRC ((), state, hdr)
    where
      err :: Show a => a -> b
      err msg = error $ "Panic! applyTick failed: " <> (show msg)

  -- | Apply the block level ledger transition.
  applyBlock ::
    MonadError (BlockTransitionError era) m =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  default applyBlock ::
    ( STS (STS.BBODY era),
      MonadError (BlockTransitionError era) m
    ) =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  applyBlock globals state blk =
    liftEither
      . right (updateNewEpochState state)
      . left (BlockTransitionError . join)
      $ res
    where
      res =
        flip runReader globals . applySTS @(STS.BBODY era) $
          TRC (mkBbodyEnv state, bbs, blk)
      bbs =
        STS.BbodyState
          (LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)

  -- | Re-apply a ledger block to the same state it has been applied to before.
  --
  -- This function does no validation of whether the block applies successfully;
  -- the caller implicitly guarantees that they have previously called
  -- 'applyBlockTransition' on the same block and that this was successful.
  reapplyBlock ::
    Globals ->
    NewEpochState era ->
    Block era ->
    NewEpochState era
  default reapplyBlock ::
    STS (STS.BBODY era) =>
    Globals ->
    NewEpochState era ->
    Block era ->
    NewEpochState era
  reapplyBlock globals state blk =
    updateNewEpochState state res
    where
      res =
        flip runReader globals . reapplySTS @(STS.BBODY era) $
          TRC (mkBbodyEnv state, bbs, blk)
      bbs =
        STS.BbodyState
          (LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)

instance PraosCrypto crypto => ApplyBlock (ShelleyEra crypto)

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}

chainChecks ::
  forall era m.
  ( Era era,
    MonadError (STS.PredicateFailure (STS.CHAIN era)) m
  ) =>
  Globals ->
  STS.ChainChecksData ->
  BHeader (Crypto era) ->
  m ()
chainChecks globals ccd bh = STS.chainChecks (maxMajorPV globals) ccd bh

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkBbodyEnv ::
  NewEpochState era ->
  STS.BbodyEnv era
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodyPp = LedgerState.esPp nesEs,
        STS.bbodyAccount = LedgerState.esAccountState nesEs
      }

updateNewEpochState ::
  NewEpochState era ->
  STS.BbodyState era ->
  NewEpochState era
updateNewEpochState ss (STS.BbodyState ls bcur) =
  LedgerState.updateNES ss bcur ls

newtype TickTransitionError era
  = TickTransitionError [STS.PredicateFailure (STS.TICK era)]
  deriving (Generic)

instance
  (NoThunks (STS.PredicateFailure (STS.TICK era))) =>
  NoThunks (TickTransitionError era)

deriving stock instance
  (Eq (STS.PredicateFailure (STS.TICK era))) =>
  Eq (TickTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (STS.TICK era))) =>
  Show (TickTransitionError era)

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
  (NoThunks (STS.PredicateFailure (STS.BBODY era))) =>
  NoThunks (BlockTransitionError era)
