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
    annotateBlock,
    applyBlockOptsAnnotated,
    AnnotatedBlock (..),
  )
where

import Cardano.Ledger.BHeaderView (BHeaderView (bhviewSlot))
import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase, epochInfo)
import Cardano.Ledger.Block (Block (..))
import qualified Cardano.Ledger.Chain as STS
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (ChainData, SerialisableData)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era, TxSeq, WellFormed, fromTxSeq)
import Cardano.Ledger.Serialization (ToCBORGroup)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.API.Protocol (PraosCrypto)
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.LedgerState (EpochState (esPp), NewEpochState (..), esLState, keyRefunds, _delegationState, _pParams, _pstate, _utxo, _utxoState)
import qualified Cardano.Ledger.Shelley.LedgerState as LedgerState
import Cardano.Ledger.Shelley.PParams (PParams' (..))
import Cardano.Ledger.Shelley.Rules.Bbody (AnnotatedBlock (..))
import qualified Cardano.Ledger.Shelley.Rules.Bbody as STS
import Cardano.Ledger.Shelley.Rules.EraMapping ()
import Cardano.Ledger.Shelley.TxBody (Wdrl (unWdrl))
import Cardano.Ledger.Shelley.UTxO (balance, totalDeposits, txouts)
import Cardano.Ledger.Slot (SlotNo (..), epochInfoSize)
import Cardano.Ledger.TxIn (TxIn)
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (epochInfoFirst, epochInfoSlotToUTCTime)
import Control.Arrow (left, right)
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans.Reader (runReader)
import Control.SetAlgebra (eval, (<|))
import Control.State.Transition.Extended
import Data.Bifunctor (second)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (fold)
import qualified Data.Foldable as Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import NoThunks.Class (NoThunks (..))

{-------------------------------------------------------------------------------
  Block validation API
-------------------------------------------------------------------------------}

class
  ( ChainData (NewEpochState era),
    SerialisableData (NewEpochState era),
    ChainData (BlockTransitionError era),
    ChainData (STS.ChainPredicateFailure era),
    STS (Core.EraRule "TICK" era),
    BaseM (Core.EraRule "TICK" era) ~ ShelleyBase,
    Environment (Core.EraRule "TICK" era) ~ (),
    State (Core.EraRule "TICK" era) ~ NewEpochState era,
    Signal (Core.EraRule "TICK" era) ~ SlotNo,
    STS (Core.EraRule "BBODY" era),
    BaseM (Core.EraRule "BBODY" era) ~ ShelleyBase,
    Environment (Core.EraRule "BBODY" era) ~ STS.BbodyEnv era,
    State (Core.EraRule "BBODY" era) ~ STS.BbodyState era,
    Signal (Core.EraRule "BBODY" era) ~ (Block BHeaderView era),
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
    (Block BHeaderView era) ->
    m (EventReturnType ep (Core.EraRule "BBODY" era) (NewEpochState era))
  default applyBlockOpts ::
    forall ep m.
    (EventReturnTypeRep ep, MonadError (BlockTransitionError era) m) =>
    ApplySTSOpts ep ->
    Globals ->
    NewEpochState era ->
    (Block BHeaderView era) ->
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
    (Block BHeaderView era) ->
    NewEpochState era
  default reapplyBlock ::
    Globals ->
    NewEpochState era ->
    (Block BHeaderView era) ->
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
  (Block BHeaderView era) ->
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
  MonadError (STS.ChainPredicateFailure era) m =>
  Globals ->
  STS.ChainChecksPParams ->
  BHeaderView (Crypto era) ->
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

annotateBlock ::
  forall era.
  ( WellFormed era,
    Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin
  ) =>
  Globals ->
  NewEpochState era ->
  (Block BHeaderView era) ->
  AnnotatedBlock
annotateBlock globals state (Block' bheaderView txseq _) =
  AnnotatedBlock
    { abEpochNo = epochNo,
      abSlotNo = slotNo,
      abEpochSlot = runIdentity $ (epochInfoFirst $ epochInfo globals) epochNo,
      abTimeStamp = runIdentity $ epochInfoSlotToUTCTime (epochInfo globals) (systemStart globals) slotNo,
      abEpochSize = runReader (epochInfoSize (epochInfo globals) epochNo) globals,
      abTxs = fmap annotateTx $ Foldable.toList $ fromTxSeq txseq
    }
  where
    slotNo = bhviewSlot bheaderView
    epochNo = nesEL state
    annotateTx :: Core.Tx era -> STS.AnnotatedTx
    annotateTx tx =
      let txBody = getField @"body" tx
          ledgerState = esLState $ nesEs state
          pp_ = esPp $ nesEs state
          u = _utxoState ledgerState
          pools = _pParams $ _pstate $ _delegationState $ ledgerState
          certs = Foldable.toList (getField @"certs" txBody)
          ins =
            Val.coin (balance @era (eval ((getField @"inputs" txBody) <| _utxo u)))
              <> keyRefunds pp_ txBody
              <> fold (unWdrl (getField @"wdrls" txBody))
          outs =
            Val.coin (balance (txouts @era txBody))
              <> getField @"txfee" txBody
              <> totalDeposits pp_ (`Map.notMember` pools) certs
       in STS.AnnotatedTx
            { STS.atInputSum = ins,
              STS.atOutSum = outs,
              STS.txFees = getField @"txfee" txBody
            }

applyBlockOptsAnnotated ::
  forall ep m era.
  ( EventReturnTypeRep ep,
    MonadError (BlockTransitionError era) m,
    Era era,
    HasField "inputs" (Core.TxBody era) (Set (TxIn (Crypto era))),
    HasField "certs" (Core.TxBody era) (StrictSeq (DCert (Crypto era))),
    HasField "wdrls" (Core.TxBody era) (Wdrl (Crypto era)),
    HasField "_keyDeposit" (Core.PParams era) Coin,
    HasField "_poolDeposit" (Core.PParams era) Coin,
    Coercible (Event (Core.EraRule "BBODY" era)) (STS.BbodyEvent era),
    ApplyBlock era
  ) =>
  ApplySTSOpts ep ->
  Globals ->
  NewEpochState era ->
  (Block BHeaderView era) ->
  m (EventReturnType ep (Core.EraRule "BBODY" era) (NewEpochState era))
applyBlockOptsAnnotated opts globals state blk = do
  res' <- applyBlockOpts opts globals state blk
  case asoEvents opts of
    EPDiscard -> pure res'
    EPReturn -> do
      let ann = annotateBlock globals state blk
      let annEv :: STS.BbodyEvent era
          annEv = STS.AnnotatedBlockEvent ann
      pure $ second (<> [coerce annEv]) res'
