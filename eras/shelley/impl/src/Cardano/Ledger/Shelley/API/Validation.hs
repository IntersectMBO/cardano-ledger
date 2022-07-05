{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Serialization (ToCBORGroup)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
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
  ( STS (EraRule "TICK" era),
    BaseM (EraRule "TICK" era) ~ ShelleyBase,
    Environment (EraRule "TICK" era) ~ (),
    State (EraRule "TICK" era) ~ NewEpochState era,
    Signal (EraRule "TICK" era) ~ SlotNo,
    STS (EraRule "BBODY" era),
    BaseM (EraRule "BBODY" era) ~ ShelleyBase,
    Environment (EraRule "BBODY" era) ~ STS.BbodyEnv era,
    State (EraRule "BBODY" era) ~ STS.BbodyState era,
    Signal (EraRule "BBODY" era) ~ Block (BHeaderView (Crypto era)) era,
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
    EventReturnType ep (EraRule "TICK" era) (NewEpochState era)
  applyTickOpts opts globals state hdr =
    either err id
      . flip runReader globals
      . applySTSOptsEither @(EraRule "TICK" era) opts
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
    m (EventReturnType ep (EraRule "BBODY" era) (NewEpochState era))
  applyBlockOpts opts globals state blk =
    liftEither
      . left BlockTransitionError
      . right
        ( mapEventReturn @ep @(EraRule "BBODY" era) $
            updateNewEpochState state
        )
      $ res
    where
      res =
        flip runReader globals
          . applySTSOptsEither @(EraRule "BBODY" era)
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
  reapplyBlock globals state blk =
    updateNewEpochState state res
    where
      res =
        flip runReader globals . reapplySTS @(EraRule "BBODY" era) $
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

{-# DEPRECATED ShelleyEraCrypto "Constraint synonyms are being removed" #-}

instance
  ( CC.Crypto crypto,
    DSignable crypto (Hash crypto EraIndependentTxBody)
  ) =>
  ApplyBlock (ShelleyEra crypto)

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
  = TickTransitionError [STS.PredicateFailure (EraRule "TICK" era)]
  deriving (Generic)

instance
  (NoThunks (STS.PredicateFailure (EraRule "TICK" era))) =>
  NoThunks (TickTransitionError era)

deriving stock instance
  (Eq (STS.PredicateFailure (EraRule "TICK" era))) =>
  Eq (TickTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (EraRule "TICK" era))) =>
  Show (TickTransitionError era)

newtype BlockTransitionError era
  = BlockTransitionError [STS.PredicateFailure (EraRule "BBODY" era)]
  deriving (Generic)

deriving stock instance
  (Eq (STS.PredicateFailure (EraRule "BBODY" era))) =>
  Eq (BlockTransitionError era)

deriving stock instance
  (Show (STS.PredicateFailure (EraRule "BBODY" era))) =>
  Show (BlockTransitionError era)

instance
  (NoThunks (STS.PredicateFailure (EraRule "BBODY" era))) =>
  NoThunks (BlockTransitionError era)
