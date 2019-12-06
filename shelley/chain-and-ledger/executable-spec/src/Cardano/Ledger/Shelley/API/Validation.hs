{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Interface to the block validation and chain extension logic in the Shelley
-- API.
module Cardano.Ledger.Shelley.API.Validation
  ( ShelleyState,
    applyHeaderTransition,
    applyBlockTransition,
  )
where

import BlockChain
import qualified Cardano.Crypto.DSIGN as DSIGN
import Cardano.Ledger.Shelley.Crypto
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.State.Transition (TRC (..), applySTS)
import Ledger.Core (Relation (..))
import qualified LedgerState
import qualified STS.Bbody as STS
import qualified STS.Tick as STS
import qualified TxData as Tx
import Slot (Slot)

-- | Type alias for the state updated by TICK and BBODY rules
type ShelleyState = LedgerState.NewEpochState

{-------------------------------------------------------------------------------
  Applying blocks
-------------------------------------------------------------------------------}

mkTickEnv ::
  ShelleyState crypto ->
  STS.TickEnv crypto
mkTickEnv = STS.TickEnv . LedgerState.getGKeys

mkBbodyEnv ::
  ShelleyState crypto ->
  STS.BbodyEnv
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesOsched,
      LedgerState.nesEs
    } = STS.BbodyEnv
    { STS.bbodySlots = dom nesOsched,
      STS.bbodyPp = LedgerState.esPp nesEs,
      STS.bbodyReserves =
        LedgerState._reserves
          . LedgerState.esAccountState
          $ nesEs
    }

newtype HeaderTransitionError crypto
  = HeaderTransitionError [STS.PredicateFailure (STS.TICK crypto)]
  deriving (Eq, Show)

-- | Apply the header level ledger transition.
--
-- This handles checks and updates that happen on a slot tick, as well as a few
-- header level checks, such as size constraints.
applyHeaderTransition ::
  forall crypto m.
  ( Crypto crypto,
    MonadError (HeaderTransitionError crypto) m
  ) =>
  ShelleyState crypto ->
  Slot ->
  m (ShelleyState crypto)
applyHeaderTransition state hdr =
  liftEither
    . left (HeaderTransitionError . join)
    . applySTS @(STS.TICK crypto)
    $ TRC (mkTickEnv state, state, hdr)

newtype BlockTransitionError crypto
  = BlockTransitionError [STS.PredicateFailure (STS.BBODY crypto)]
  deriving (Eq, Show)

-- | Apply the block level ledger transition.
applyBlockTransition ::
  forall crypto m.
  ( Crypto crypto,
    MonadError (BlockTransitionError crypto) m,
    DSIGN.Signable (DSIGN crypto) (Tx.TxBody crypto)
  ) =>
  ShelleyState crypto ->
  Block crypto ->
  m (ShelleyState crypto)
applyBlockTransition state blk =
  liftEither
    . right (updateShelleyState state)
    . left (BlockTransitionError . join)
    . applySTS @(STS.BBODY crypto)
    $ TRC (mkBbodyEnv state, bbs, blk)
  where
    updateShelleyState ::
      ShelleyState crypto ->
      STS.BbodyState crypto ->
      ShelleyState crypto
    updateShelleyState ss (STS.BbodyState ls bcur) =
      LedgerState.updateNES ss bcur ls
    bbs =
      STS.BbodyState
        (LedgerState.esLState $ LedgerState.nesEs state)
        (LedgerState.nesBcur state)
