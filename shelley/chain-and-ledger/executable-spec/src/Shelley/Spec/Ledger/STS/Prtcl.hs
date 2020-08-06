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
    mkNonceFromOutputVRF,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    BHeader (..),
    LastAppliedBlock (..),
    PrevHash,
    bhbody,
    lastAppliedHash,
  )
import Shelley.Spec.Ledger.Crypto (Crypto, VRF)
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    KESignable,
    KeyHash,
    KeyRole (..),
    VRFSignable,
  )
import Shelley.Spec.Ledger.LedgerState (OBftSlot)
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.STS.Overlay (OVERLAY, OverlayEnv (..))
import Shelley.Spec.Ledger.STS.Updn (UPDN, UpdnEnv (..), UpdnState (..))
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)
import Shelley.Spec.Ledger.Slot (BlockNo, SlotNo)
import Shelley.Spec.Ledger.Value

data PRTCL crypto v

data PrtclState crypto
  = PrtclState
      !(Map (KeyHash 'BlockIssuer crypto) Word64)
      -- ^ Operation Certificate counters
      !Nonce
      -- ^ Evolving nonce
      !Nonce
      -- ^ Candidate nonce
  deriving (Generic, Show, Eq)

instance Crypto crypto => ToCBOR (PrtclState crypto) where
  toCBOR (PrtclState m n1 n2) =
    mconcat
      [ encodeListLen 3,
        toCBOR m,
        toCBOR n1,
        toCBOR n2
      ]

instance Crypto crypto => FromCBOR (PrtclState crypto) where
  fromCBOR =
    decodeRecordNamed
      "PrtclState"
      (const 3)
      ( PrtclState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance Crypto crypto => NoUnexpectedThunks (PrtclState crypto)

data PrtclEnv crypto
  = PrtclEnv
      (Map SlotNo (OBftSlot crypto))
      (PoolDistr crypto)
      (GenDelegs crypto)
      Nonce
  deriving (Generic)

instance NoUnexpectedThunks (PrtclEnv crypto)

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto v),
    VRFSignable crypto Seed
  ) =>
  STS (PRTCL crypto v)
  where
  type
    State (PRTCL crypto v) =
      PrtclState crypto

  type
    Signal (PRTCL crypto v) =
      BHeader crypto v

  type
    Environment (PRTCL crypto v) =
      PrtclEnv crypto

  type BaseM (PRTCL crypto v) = ShelleyBase

  data PredicateFailure (PRTCL crypto v)
    = OverlayFailure (PredicateFailure (OVERLAY crypto v)) -- Subtransition Failures
    | UpdnFailure (PredicateFailure (UPDN crypto)) -- Subtransition Failures
    deriving (Generic)

  initialRules = []

  transitionRules = [prtclTransition]

deriving instance (VRF.VRFAlgorithm (VRF crypto)) => Show (PredicateFailure (PRTCL crypto v))

deriving instance (VRF.VRFAlgorithm (VRF crypto)) => Eq (PredicateFailure (PRTCL crypto v))

prtclTransition ::
  forall crypto v.
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto v),
    VRFSignable crypto Seed
  ) =>
  TransitionRule (PRTCL crypto v)
prtclTransition = do
  TRC
    ( PrtclEnv osched pd dms eta0,
      PrtclState cs etaV etaC,
      bh
      ) <-
    judgmentContext
  let bhb = bhbody bh
      slot = bheaderSlotNo bhb
      eta = mkNonceFromOutputVRF . VRF.certifiedOutput $ bheaderEta bhb

  UpdnState etaV' etaC' <-
    trans @(UPDN crypto) $
      TRC
        ( UpdnEnv eta,
          UpdnState etaV etaC,
          slot
        )
  cs' <-
    trans @(OVERLAY crypto v) $
      TRC (OverlayEnv osched eta0 pd dms, cs, bh)

  pure $
    PrtclState
      cs'
      etaV'
      etaC'

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (PRTCL crypto v))

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto v),
    VRFSignable crypto Seed
  ) =>
  Embed (OVERLAY crypto v) (PRTCL crypto v)
  where
  wrapFailed = OverlayFailure

instance
  ( CV crypto v,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto v),
    VRFSignable crypto Seed
  ) =>
  Embed (UPDN crypto) (PRTCL crypto v)
  where
  wrapFailed = UpdnFailure

data PrtlSeqFailure crypto v
  = WrongSlotIntervalPrtclSeq
      SlotNo
      -- ^ Last slot number.
      SlotNo
      -- ^ Current slot number.
  | WrongBlockNoPrtclSeq
      (WithOrigin (LastAppliedBlock crypto v))
      -- ^ Last applied block.
      BlockNo
      -- ^ Current block number.
  | WrongBlockSequencePrtclSeq
      (PrevHash crypto v)
      -- ^ Last applied hash
      (PrevHash crypto v)
      -- ^ Current block's previous hash
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoUnexpectedThunks (PrtlSeqFailure crypto v)

prtlSeqChecks ::
  (MonadError (PrtlSeqFailure crypto v) m, CV crypto v) =>
  WithOrigin (LastAppliedBlock crypto v) ->
  BHeader crypto v ->
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
