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

-- | Interface to the block validation and chain extension logic in the Shelley
-- API.
module Cardano.Ledger.Shelley.API.Validation (
  ApplyBlock (..),
  applyBlock,
  applyTick,
  TickTransitionError (..),
  BlockTransitionError (..),
  chainChecks,
  ShelleyEraCrypto,
)
where

import Cardano.Ledger.BHeaderView (BHeaderView)
import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase, Version)
import Cardano.Ledger.Binary (EncCBORGroup)
import Cardano.Ledger.Block (Block)
import qualified Cardano.Ledger.Chain as STS
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core (EraGov)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), NewEpochState, curPParamsEpochStateL)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.PParams ()
import Cardano.Ledger.Shelley.Rules ()
import qualified Cardano.Ledger.Shelley.Rules as STS
import Cardano.Ledger.Slot (SlotNo)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}

class
  ( STS (EraRule "TICK" era)
  , BaseM (EraRule "TICK" era) ~ ShelleyBase
  , Environment (EraRule "TICK" era) ~ ()
  , State (EraRule "TICK" era) ~ NewEpochState era
  , Signal (EraRule "TICK" era) ~ SlotNo
  , STS (EraRule "BBODY" era)
  , BaseM (EraRule "BBODY" era) ~ ShelleyBase
  , Environment (EraRule "BBODY" era) ~ STS.BbodyEnv era
  , State (EraRule "BBODY" era) ~ STS.ShelleyBbodyState era
  , Signal (EraRule "BBODY" era) ~ Block (BHeaderView (EraCrypto era)) era
  , EncCBORGroup (TxSeq era)
  , State (EraRule "LEDGERS" era) ~ LedgerState era
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
    Block (BHeaderView (EraCrypto era)) era ->
    m (EventReturnType ep (EraRule "BBODY" era) (NewEpochState era))
  default applyBlockOpts ::
    forall ep m.
    (EventReturnTypeRep ep, MonadError (BlockTransitionError era) m, EraGov era) =>
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    Block (BHeaderView (EraCrypto era)) era ->
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
    Block (BHeaderView (EraCrypto era)) era ->
    NewEpochState era
  default reapplyBlock ::
    EraGov era =>
    Globals ->
    NewEpochState era ->
    Block (BHeaderView (EraCrypto era)) era ->
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
      { asoAssertions = globalAssertionPolicy
      , asoValidation = ValidateAll
      , asoEvents = EPDiscard
      }

applyBlock ::
  ( ApplyBlock era
  , MonadError (BlockTransitionError era) m
  ) =>
  Globals ->
  NewEpochState era ->
  Block (BHeaderView (EraCrypto era)) era ->
  m (NewEpochState era)
applyBlock =
  applyBlockOpts $
    ApplySTSOpts
      { asoAssertions = globalAssertionPolicy
      , asoValidation = ValidateAll
      , asoEvents = EPDiscard
      }

type ShelleyEraCrypto c =
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  )

{-# DEPRECATED ShelleyEraCrypto "Constraint synonyms are being removed" #-}

instance
  ( Crypto c
  , DSignable c (Hash c EraIndependentTxBody)
  ) =>
  ApplyBlock (ShelleyEra c)

{-------------------------------------------------------------------------------
  CHAIN Transition checks
-------------------------------------------------------------------------------}

chainChecks ::
  forall c m.
  MonadError STS.ChainPredicateFailure m =>
  -- | Max major protocol version
  Version ->
  STS.ChainChecksPParams ->
  BHeaderView c ->
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
      , STS.bbodyAccount = LedgerState.esAccountState nesEs
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
