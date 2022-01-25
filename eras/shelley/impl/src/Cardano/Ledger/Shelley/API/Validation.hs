{-# LANGUAGE ConstraintKinds #-}
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
module Cardano.Ledger.Shelley.API.Validation
  ( ApplyBlock (..),
    applyBlock,
    applyTick,
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
    ShelleyEraCrypto,
  )
where

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Cardano.Ledger.Block (Block)
import qualified Cardano.Ledger.Chain as STS
import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, TxSeq)
import Cardano.Ledger.Hashes (EraIndependentTxBody)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Serialization (ToCBORGroup)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import qualified Cardano.Ledger.Shelley.Rules.Bbody as STS
import Cardano.Ledger.Shelley.Rules.EraMapping ()
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}

class
  ( ChainData (NewEpochState era),
    SerialisableData (NewEpochState era),
    ChainData (BlockTransitionError era),
    ChainData STS.ChainPredicateFailure,
    STS (Core.EraRule "TICK" era),
    BaseM (Core.EraRule "TICK" era) ~ ShelleyBase,
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    STS (Core.EraRule "BBODY" era),
    BaseM (Core.EraRule "BBODY" era) ~ ShelleyBase,
    Environment (Core.EraRule "BBODY" era) ~ STS.BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ STS.BbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ Block (BHeaderView (Crypto era)) era,
    ToCBORGroup (TxSeq era)
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
    Block (BHeaderView (Crypto era)) era ->
    m (EventReturnType ep (Core.EraRule "BBODY" era) (NewEpochState era))
  default applyBlockOpts ::
    forall ep m.
    (EventReturnTypeRep ep, MonadError (BlockTransitionError era) m) =>
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    Block (BHeaderView (Crypto era)) era ->
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
    Block (BHeaderView (Crypto era)) era ->
    NewEpochState era
  default reapplyBlock ::
    Globals ->
    NewEpochState era ->
    Block (BHeaderView (Crypto era)) era ->
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
  Block (BHeaderView (Crypto era)) era ->
  m (NewEpochState era)
applyBlock =
  applyBlockOpts $
    ApplySTSOpts
      { asoAssertions = globalAssertionPolicy,
        asoValidation = ValidateAll,
        asoEvents = EPDiscard
      }

type ShelleyEraCrypto crypto =
  ( CC.Crypto crypto,
    DSignable crypto (Hash crypto EraIndependentTxBody)
  )

instance ShelleyEraCrypto crypto => ApplyBlock (ShelleyEra crypto)

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}

chainChecks ::
  forall crypto m.
  MonadError STS.ChainPredicateFailure m =>
  -- | Max major protocol version
  Natural ->
  STS.ChainChecksPParams ->
  BHeaderView crypto ->
  m ()
chainChecks = STS.chainChecks

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
