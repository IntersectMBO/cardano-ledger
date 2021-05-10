{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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

import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Cardano.Ledger.Core (AnnotatedData, ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyEra)
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
import Shelley.Spec.Ledger.Slot (SlotNo)
import Data.Bifunctor

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
  applyTickOpts
    :: ApplySTSOpts
    -> Globals
    -> NewEpochState era
    -> SlotNo
    -> (NewEpochState era, [[PredicateFailure (Core.EraRule "TICK" era)]])
  default applyTickOpts
    :: ApplySTSOpts
    -> Globals
    -> NewEpochState era
    -> SlotNo
    -> (NewEpochState era, [[PredicateFailure (Core.EraRule "TICK" era)]])
  applyTickOpts opts globals state hdr = flip runReader globals $ applySTSOpts @(Core.EraRule "TICK" era) opts
      ( TRC ((), state, hdr) )
  {-# INLINE applyTickOpts #-}

  -- | Apply the block level ledger transition.
  applyBlockOpts
    :: ApplySTSOpts
    -> Globals
    -> NewEpochState era
    -> Block era
    -> (NewEpochState era, [[PredicateFailure (Core.EraRule "BBODY" era)]])
  applyBlockOpts opts globals state blk
      = first (updateNewEpochState state)
      $ flip runReader globals
      $ applySTSOpts @(Core.EraRule "BBODY" era) opts
      $ TRC (mkBbodyEnv state, bbs, blk)
    where
      bbs = getBBodyState state
  {-# INLINE applyBlockOpts #-}


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
    Globals ->
    NewEpochState era ->
    SlotNo ->
    NewEpochState era
  applyTick globals state hdr = case applyTickOpts defaultOpts globals state hdr of
        (st, []) -> st
        (_, pfs) -> error $ "Panic! applyTick failed: " <> show pfs
    where
      defaultOpts = ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateAll
        }
  {-# INLINE applyTick #-}

  -- | Apply the block level ledger transition.
  applyBlock ::
    MonadError (BlockTransitionError era) m =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  default applyBlock ::
    (MonadError (BlockTransitionError era) m) =>
    Globals ->
    NewEpochState era ->
    Block era ->
    m (NewEpochState era)
  applyBlock globals state blk = liftEither $ case applyBlockOpts defaultOpts globals state blk of
        (st, []) -> Right st
        (_, pfs) -> Left . BlockTransitionError $ join pfs
    where
      defaultOpts = ApplySTSOpts
        { asoAssertions = AssertionsOff,
          asoValidation = ValidateAll
        }
  {-# INLINE applyBlock #-}

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
  reapplyBlock globals state blk = runReader reapplyBlock' globals
    where
      reapplyBlock' =
        updateNewEpochState state <$> res
      res = reapplySTS @(Core.EraRule "BBODY" era) $
          TRC (mkBbodyEnv state, bbs, blk)
      bbs = getBBodyState state
  {-# INLINE reapplyBlock #-}

getBBodyState :: NewEpochState era -> STS.BbodyState era
getBBodyState state = STS.BbodyState
        (LedgerState.esLState $ LedgerState.nesEs state)
        (LedgerState.nesBcur state)
{-# INLINE getBBodyState #-}

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
