{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Prtcl
  ( PRTCL,
    State,
    PrtclEnv (..),
    PrtclState (..),
    PredicateFailure (..),
    PrtlSeqFailure (..),
    prtlSeqChecks,
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    encodeListLen,
  )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.Crypto (VRF)
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Prelude (MonadError (..), NoUnexpectedThunks (..), unless)
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.State.Transition
import Data.Map.Strict (Map)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
  ( Nonce,
    Seed,
    ShelleyBase,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    BHeader (..),
    LastAppliedBlock (..),
    PrevHash,
    bhbody,
    bnonce,
    lastAppliedHash,
  )
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    KESignable,
    KeyHash,
    KeyRole (..),
    VRFSignable,
  )
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.OverlaySchedule (OverlaySchedule)
import Shelley.Spec.Ledger.STS.Overlay (OVERLAY, OverlayEnv (..))
import Shelley.Spec.Ledger.STS.Updn (UPDN, UpdnEnv (..), UpdnState (..))
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.Slot (BlockNo, SlotNo)

data PRTCL era

data PrtclState era
  = PrtclState
      !(Map (KeyHash 'BlockIssuer era) Word64)
      -- ^ Operation Certificate counters
      !Nonce
      -- ^ Evolving nonce
      !Nonce
      -- ^ Candidate nonce
  deriving (Generic, Show, Eq)

instance Era era => ToCBOR (PrtclState era) where
  toCBOR (PrtclState m n1 n2) =
    mconcat
      [ encodeListLen 3,
        toCBOR m,
        toCBOR n1,
        toCBOR n2
      ]

instance Era era => FromCBOR (PrtclState era) where
  fromCBOR =
    decodeRecordNamed
      "PrtclState"
      (const 3)
      ( PrtclState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance Era era => NoUnexpectedThunks (PrtclState era)

data PrtclEnv era
  = PrtclEnv
      (OverlaySchedule era)
      (PoolDistr era)
      (GenDelegs era)
      Nonce
  deriving (Generic)

instance NoUnexpectedThunks (PrtclEnv era)

instance
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRFSignable era Seed
  ) =>
  STS (PRTCL era)
  where
  type
    State (PRTCL era) =
      PrtclState era

  type
    Signal (PRTCL era) =
      BHeader era

  type
    Environment (PRTCL era) =
      PrtclEnv era

  type BaseM (PRTCL era) = ShelleyBase

  data PredicateFailure (PRTCL era)
    = OverlayFailure (PredicateFailure (OVERLAY era)) -- Subtransition Failures
    | UpdnFailure (PredicateFailure (UPDN era)) -- Subtransition Failures
    deriving (Generic)

  initialRules = []

  transitionRules = [prtclTransition]

deriving instance (VRF.VRFAlgorithm (VRF (Crypto era))) => Show (PredicateFailure (PRTCL era))

deriving instance (VRF.VRFAlgorithm (VRF (Crypto era))) => Eq (PredicateFailure (PRTCL era))

prtclTransition ::
  forall era.
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRFSignable era Seed
  ) =>
  TransitionRule (PRTCL era)
prtclTransition = do
  TRC
    ( PrtclEnv osched pd dms eta0,
      PrtclState cs etaV etaC,
      bh
      ) <-
    judgmentContext
  let bhb = bhbody bh
      slot = bheaderSlotNo bhb
      eta = bnonce bhb

  UpdnState etaV' etaC' <-
    trans @(UPDN era) $
      TRC
        ( UpdnEnv eta,
          UpdnState etaV etaC,
          slot
        )
  cs' <-
    trans @(OVERLAY era) $
      TRC (OverlayEnv osched pd dms eta0, cs, bh)

  pure $
    PrtclState
      cs'
      etaV'
      etaC'

instance (Era era) => NoUnexpectedThunks (PredicateFailure (PRTCL era))

instance
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRFSignable era Seed
  ) =>
  Embed (OVERLAY era) (PRTCL era)
  where
  wrapFailed = OverlayFailure

instance
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRFSignable era Seed
  ) =>
  Embed (UPDN era) (PRTCL era)
  where
  wrapFailed = UpdnFailure

data PrtlSeqFailure era
  = WrongSlotIntervalPrtclSeq
      SlotNo
      -- ^ Last slot number.
      SlotNo
      -- ^ Current slot number.
  | WrongBlockNoPrtclSeq
      (WithOrigin (LastAppliedBlock era))
      -- ^ Last applied block.
      BlockNo
      -- ^ Current block number.
  | WrongBlockSequencePrtclSeq
      (PrevHash era)
      -- ^ Last applied hash
      (PrevHash era)
      -- ^ Current block's previous hash
  deriving (Show, Eq, Generic)

instance Era era => NoUnexpectedThunks (PrtlSeqFailure era)

prtlSeqChecks ::
  (MonadError (PrtlSeqFailure era) m, Era era) =>
  WithOrigin (LastAppliedBlock era) ->
  BHeader era ->
  m ()
prtlSeqChecks lab bh =
  case lab of
    Origin -> pure ()
    At (LastAppliedBlock bL sL _) -> do
      unless (sL < slot) . throwError $ WrongSlotIntervalPrtclSeq sL slot
      unless (bL + 1 == bn) . throwError $ WrongBlockNoPrtclSeq lab bn
      unless (ph == bheaderPrev bhb) . throwError $ WrongBlockSequencePrtclSeq ph (bheaderPrev bhb)
  where
    bhb = bhbody bh
    bn = bheaderBlockNo bhb
    slot = bheaderSlotNo bhb
    ph = lastAppliedHash lab
