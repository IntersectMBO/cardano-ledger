{-# LANGUAGE DataKinds #-}
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
    applyBlock,
    applyTick,
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
  )
where

import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.API.Protocol (PraosCrypto)
import Shelley.Spec.Ledger.BlockChain (BHeader, Block)
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import Shelley.Spec.Ledger.PParams (PParams' (..))
import qualified Shelley.Spec.Ledger.STS.Bbody as STS
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import Shelley.Spec.Ledger.STS.EraMapping ()

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
    ChainData (STS.PredicateFailure (STS.CHAIN era)),
    STS (Core.EraRule "TICK" era),
    BaseM (Core.EraRule "TICK" era) ~ ShelleyBase,
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    STS (Core.EraRule "BBODY" era),
    BaseM (Core.EraRule "BBODY" era) ~ ShelleyBase,
    Environment (Core.EraRule "BBODY" era) ~ STS.BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ STS.BbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ Block era
  ) =>
  ApplyBlock era
  where
  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTickOpts ::
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    SlotNo ->
    EventReturnType ep (Core.EraRule "TICK" era) (NewEpochState era)
  default applyTickOpts ::
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    SlotNo ->
    EventReturnType ep (Core.EraRule "TICK" era) (NewEpochState era)
  applyTickOpts opts globals state hdr =
    either err id
      . flip runReader globals
      . applySTSOptsEither @(Core.EraRule "TICK" era) opts
      $ TRC ((), state, hdr)
    where
      err :: Show a => a -> b
      err msg = error $ "Panic! applyTick failed: " <> show msg

  -- | Apply the block level ledger transition.
  applyBlockOpts ::
    forall ep m.
    (EventReturnTypeRep ep, MonadError (BlockTransitionError era) m) =>
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    Block era ->
    m (EventReturnType ep (Core.EraRule "BBODY" era) (NewEpochState era))
  default applyBlockOpts ::
    forall ep m.
    (EventReturnTypeRep ep, MonadError (BlockTransitionError era) m) =>
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    Block era ->
    m (EventReturnType ep (Core.EraRule "BBODY" era) (NewEpochState era))
  applyBlockOpts opts globals state blk =
    liftEither
      . left BlockTransitionError
      . right
        ( mapEventReturn @ep @(Core.EraRule "BBODY" era) $
            updateNewEpochState state
        )
      $ res
    where
      res =
        flip runReader globals
          . applySTSOptsEither @(Core.EraRule "BBODY" era)
            opts
          $ TRC (mkBbodyEnv state, bbs, blk)
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
    Globals ->
    NewEpochState era ->
    Block era ->
    NewEpochState era
  reapplyBlock globals state blk =
    updateNewEpochState state res
    where
      res =
        flip runReader globals . reapplySTS @(Core.EraRule "BBODY" era) $
          TRC (mkBbodyEnv state, bbs, blk)
      bbs =
        STS.BbodyState
          (LedgerState.esLState $ LedgerState.nesEs state)
          (LedgerState.nesBcur state)

applyTick ::
  ApplyBlock era =>
  Globals ->
  NewEpochState era ->
  SlotNo ->
  NewEpochState era
applyTick =
  applyTickOpts $
    ApplySTSOpts
      { asoAssertions = globalAssertionPolicy,
        asoValidation = ValidateAll,
        asoEvents = EPDiscard
      }

applyBlock ::
  ( ApplyBlock era,
    MonadError (BlockTransitionError era) m
  ) =>
  Globals ->
  NewEpochState era ->
  Block era ->
  m (NewEpochState era)
applyBlock =
  applyBlockOpts $
    ApplySTSOpts
      { asoAssertions = globalAssertionPolicy,
        asoValidation = ValidateAll,
        asoEvents = EPDiscard
      }

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
  = TickTransitionError [STS.PredicateFailure (Core.EraRule "TICK" era)]
  deriving (Generic)

instance
  (NoThunks (STS.PredicateFailure (Core.EraRule "TICK" era))) =>
  NoThunks (TickTransitionError era)

deriving stock instance
  (Eq (STS.PredicateFailure (Core.EraRule "TICK" era))) =>
  Eq (TickTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (Core.EraRule "TICK" era))) =>
  Show (TickTransitionError era)

newtype BlockTransitionError era
  = BlockTransitionError [STS.PredicateFailure (Core.EraRule "BBODY" era)]
  deriving (Generic)

deriving stock instance
  (Eq (STS.PredicateFailure (Core.EraRule "BBODY" era))) =>
  Eq (BlockTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (Core.EraRule "BBODY" era))) =>
  Show (BlockTransitionError era)

instance
  (NoThunks (STS.PredicateFailure (Core.EraRule "BBODY" era))) =>
  NoThunks (BlockTransitionError era)
