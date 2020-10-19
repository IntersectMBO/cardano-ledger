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
  ( ShelleyState,
    ApplyBlock (..),
    TickTransitionError (..),
    BlockTransitionError (..),
    chainChecks,
  )
where

import Cardano.Binary (Annotator, FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.Hash (Hash)
import Cardano.Ledger.Crypto (HASH)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Shelley (ShelleyBased, ShelleyEra)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Trans.Reader (runReader)
import Control.State.Transition.Extended
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Keys (DSignable)
import qualified Shelley.Spec.Ledger.LedgerState as LedgerState
import qualified Shelley.Spec.Ledger.STS.Bbody as STS
import qualified Shelley.Spec.Ledger.STS.Chain as STS
import qualified Shelley.Spec.Ledger.STS.Tick as STS
import Shelley.Spec.Ledger.Slot (SlotNo)
import Shelley.Spec.Ledger.TxBody (EraIndependentTxBody)

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}

-- | Type alias for the state updated by TICK and BBODY rules
type ShelleyState = LedgerState.NewEpochState

class
  ( Eq (Block era),
    Show (Block era),
    NoThunks (Block era),
    FromCBOR (Annotator (Block era)),
    ToCBOR (Block era),
    Eq (BHeader (Crypto era)),
    Show (BHeader (Crypto era)),
    NoThunks (BHeader (Crypto era)),
    FromCBOR (Annotator (BHeader (Crypto era))),
    ToCBOR (Block era),
    Eq (ShelleyState era),
    Show (ShelleyState era),
    NoThunks (ShelleyState era),
    FromCBOR (ShelleyState era),
    ToCBOR (ShelleyState era),
    Eq (BlockTransitionError era),
    Show (BlockTransitionError era),
    NoThunks (BlockTransitionError era),
    Eq (STS.PredicateFailure (STS.CHAIN era)),
    Show (STS.PredicateFailure (STS.CHAIN era)),
    NoThunks (STS.PredicateFailure (STS.CHAIN era))
  ) =>
  ApplyBlock era
  where
  -- | Apply the header level ledger transition.
  --
  -- This handles checks and updates that happen on a slot tick, as well as a
  -- few header level checks, such as size constraints.
  applyTick ::
    Globals ->
    ShelleyState era ->
    SlotNo ->
    ShelleyState era
  default applyTick ::
    ShelleyBased era =>
    Globals ->
    ShelleyState era ->
    SlotNo ->
    ShelleyState era
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
    ShelleyState era ->
    Block era ->
    m (ShelleyState era)
  default applyBlock ::
    ( STS (STS.BBODY era),
      MonadError (BlockTransitionError era) m
    ) =>
    Globals ->
    ShelleyState era ->
    Block era ->
    m (ShelleyState era)
  applyBlock globals state blk =
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
  -- This function does no validation of whether the block applies successfully;
  -- the caller implicitly guarantees that they have previously called
  -- 'applyBlockTransition' on the same block and that this was successful.
  reapplyBlock ::
    Globals ->
    ShelleyState era ->
    Block era ->
    ShelleyState era
  default reapplyBlock ::
    STS (STS.BBODY era) =>
    Globals ->
    ShelleyState era ->
    Block era ->
    ShelleyState era
  reapplyBlock globals state blk =
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

instance
  ( CC.Crypto crypto,
    DSignable crypto (Hash (HASH crypto) EraIndependentTxBody)
  ) =>
  ApplyBlock (ShelleyEra crypto)

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
  ShelleyState era ->
  STS.BbodyEnv era
mkBbodyEnv
  LedgerState.NewEpochState
    { LedgerState.nesEs
    } =
    STS.BbodyEnv
      { STS.bbodyPp = LedgerState.esPp nesEs,
        STS.bbodyAccount = LedgerState.esAccountState nesEs
      }

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
