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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Interface to the block validation and chain extension logic in the Shelley
-- API.
module Cardano.Ledger.Shelley.API.Validation (
  ApplyBlock (..),
  applyBlockEither,
  applyBlockEitherNoEvents,
  applyBlockNoValidaton,
  applyTickNoEvents,
  TickTransitionError (..),
  BlockTransitionError (..),
  chainChecks,
)
where

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase, Version)
import Cardano.Ledger.Block (Block)
import qualified Cardano.Ledger.Chain as STS
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), NewEpochState, curPParamsEpochStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.Shelley.Rules ()
import qualified Cardano.Ledger.Shelley.Rules as STS
import Cardano.Ledger.Shelley.State ()
import Cardano.Ledger.Slot (SlotNo)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}

class (EraGov era, EraSegWits era) => ApplyBlock era where
  -- | Run the `BBODY` rule with `globalAssertionPolicy`. This function always succeeds, but
  -- whenever validation is turned on it is necessary to check for presence of predicate failures
  -- before a call can be marked successful. Therefore it is recommended to call `applyBlockEither`
  -- instead.
  applyBlock ::
    SingEP ep ->
    ValidationPolicy ->
    Globals ->
    NewEpochState era ->
    Block BHeaderView era ->
    (NewEpochState era, [PredicateFailure (EraRule "BBODY" era)], [Event (EraRule "BBODY" era)])
  default applyBlock ::
    ( STS (EraRule "BBODY" era)
    , BaseM (EraRule "BBODY" era) ~ ShelleyBase
    , Environment (EraRule "BBODY" era) ~ STS.BbodyEnv era
    , State (EraRule "BBODY" era) ~ STS.ShelleyBbodyState era
    , Signal (EraRule "BBODY" era) ~ Block BHeaderView era
    , State (EraRule "LEDGERS" era) ~ LedgerState era
    ) =>
    SingEP ep ->
    ValidationPolicy ->
    Globals ->
    NewEpochState era ->
    Block BHeaderView era ->
    (NewEpochState era, [PredicateFailure (EraRule "BBODY" era)], [Event (EraRule "BBODY" era)])
  applyBlock eventsPolicy validationPolicy globals newEpochState block =
    (updateNewEpochState newEpochState stsResultState, stsResultFailures, stsResultEvents)
    where
      opts =
        ApplySTSOpts
          { asoAssertions = globalAssertionPolicy
          , asoValidation = validationPolicy
          , asoEvents = eventsPolicy
          }
      STSResult {stsResultState, stsResultFailures, stsResultEvents} =
        flip runReader globals $
          applySTSOptsResult @(EraRule "BBODY" era) opts $
            TRC (mkBbodyEnv newEpochState, bBodyState, block)
      bBodyState =
        STS.BbodyState
          (LedgerState.esLState $ LedgerState.nesEs newEpochState)
          (LedgerState.nesBcur newEpochState)

  -- | Run the `TICK` rule with `globalAssertionPolicy` and without any validation, since it can't
  -- fail anyways.
  applyTick ::
    SingEP ep ->
    Globals ->
    NewEpochState era ->
    SlotNo ->
    (NewEpochState era, [Event (EraRule "TICK" era)])
  default applyTick ::
    ( STS (EraRule "TICK" era)
    , BaseM (EraRule "TICK" era) ~ ShelleyBase
    , Environment (EraRule "TICK" era) ~ ()
    , State (EraRule "TICK" era) ~ NewEpochState era
    , Signal (EraRule "TICK" era) ~ SlotNo
    ) =>
    SingEP ep ->
    Globals ->
    NewEpochState era ->
    SlotNo ->
    (NewEpochState era, [Event (EraRule "TICK" era)])
  applyTick eventsPolicy globals newEpochState slotNo = (stsResultState, stsResultEvents)
    where
      opts =
        ApplySTSOpts
          { asoAssertions = globalAssertionPolicy
          , asoValidation = ValidateNone
          , asoEvents = eventsPolicy
          }
      STSResult {stsResultState, stsResultEvents} =
        flip runReader globals . applySTSOptsResult @(EraRule "TICK" era) opts $
          TRC ((), newEpochState, slotNo)

-- | Same as `applyBlock`, except it produces a Left when there are failures present and `Right`
-- with result otherwise.
applyBlockEither ::
  ApplyBlock era =>
  SingEP ep ->
  ValidationPolicy ->
  Globals ->
  NewEpochState era ->
  Block BHeaderView era ->
  Either (BlockTransitionError era) (NewEpochState era, [Event (EraRule "BBODY" era)])
applyBlockEither eventsPolicy validationPolicy globals newEpochState block =
  case failure of
    [] -> Right (newEpochStateResult, events)
    f : fs -> Left $ BlockTransitionError $ f :| fs
  where
    (newEpochStateResult, failure, events) =
      applyBlock eventsPolicy validationPolicy globals newEpochState block

applyBlockEitherNoEvents ::
  ApplyBlock era =>
  ValidationPolicy ->
  Globals ->
  NewEpochState era ->
  Block BHeaderView era ->
  Either (BlockTransitionError era) (NewEpochState era)
applyBlockEitherNoEvents validationPolicy globals newEpochState block =
  fst <$> applyBlockEither EPDiscard validationPolicy globals newEpochState block

-- | Re-apply a ledger block to the same state it has been applied to before.
--
-- This function does no validation of whether the block applies successfully;
-- the caller implicitly guarantees that they have previously called
-- 'applyBlockTransition' on the same block and that this was successful.
applyBlockNoValidaton ::
  ApplyBlock era =>
  Globals ->
  NewEpochState era ->
  Block BHeaderView era ->
  NewEpochState era
applyBlockNoValidaton globals newEpochState block = newEpochStateResult
  where
    (newEpochStateResult, _failure, _events) =
      applyBlock EPDiscard ValidateNone globals newEpochState block

-- | Same as `applyTick`, but do not retain any ledger events
applyTickNoEvents ::
  ApplyBlock era =>
  Globals ->
  NewEpochState era ->
  SlotNo ->
  NewEpochState era
applyTickNoEvents globals newEpochState slotNo =
  fst $ applyTick EPDiscard globals newEpochState slotNo

instance ApplyBlock ShelleyEra

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}

chainChecks ::
  forall m.
  MonadError STS.ChainPredicateFailure m =>
  -- | Max major protocol version
  Version ->
  STS.ChainChecksPParams ->
  BHeaderView ->
  m ()
chainChecks = STS.chainChecks

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkBbodyEnv ::
  EraGov era =>
  NewEpochState era ->
  STS.BbodyEnv era
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodyPp = nesEs ^. curPParamsEpochStateL
      , STS.bbodyAccount = LedgerState.esChainAccountState nesEs
      }

updateNewEpochState ::
  (LedgerState era ~ State (EraRule "LEDGERS" era), EraGov era) =>
  NewEpochState era ->
  STS.ShelleyBbodyState era ->
  NewEpochState era
updateNewEpochState ss (STS.BbodyState ls bcur) =
  LedgerState.updateNES ss bcur ls

newtype TickTransitionError era
  = TickTransitionError (NonEmpty (STS.PredicateFailure (EraRule "TICK" era)))
  deriving (Generic)

instance
  NoThunks (STS.PredicateFailure (EraRule "TICK" era)) =>
  NoThunks (TickTransitionError era)

deriving stock instance
  Eq (STS.PredicateFailure (EraRule "TICK" era)) =>
  Eq (TickTransitionError era)

deriving stock instance
  Show (STS.PredicateFailure (EraRule "TICK" era)) =>
  Show (TickTransitionError era)

newtype BlockTransitionError era
  = BlockTransitionError (NonEmpty (STS.PredicateFailure (EraRule "BBODY" era)))
  deriving (Generic)

deriving stock instance
  Eq (STS.PredicateFailure (EraRule "BBODY" era)) =>
  Eq (BlockTransitionError era)

deriving stock instance
  Show (STS.PredicateFailure (EraRule "BBODY" era)) =>
  Show (BlockTransitionError era)

instance
  NoThunks (STS.PredicateFailure (EraRule "BBODY" era)) =>
  NoThunks (BlockTransitionError era)
