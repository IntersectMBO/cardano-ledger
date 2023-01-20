{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Protocol.TPraos.Rules.Prtcl (
  PRTCL,
  State,
  PrtclEnv (..),
  PrtclState (..),
  PrtclPredicateFailure (..),
  PredicateFailure,
  PrtlSeqFailure (..),
  prtlSeqChecks,
)
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BaseTypes (
  Nonce,
  Seed,
  ShelleyBase,
  UnitInterval,
 )
import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import Cardano.Ledger.Binary.Plain (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Crypto (Crypto, VRF)
import Cardano.Ledger.Keys (
  DSignable,
  GenDelegs (..),
  KESignable,
  KeyHash,
  KeyRole (..),
  VRFSignable,
 )
import Cardano.Ledger.PoolDistr (PoolDistr)
import Cardano.Ledger.Slot (BlockNo, SlotNo)
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader (..),
  LastAppliedBlock (..),
  PrevHash,
  bhbody,
  bnonce,
  lastAppliedHash,
 )
import Cardano.Protocol.TPraos.OCert (OCertSignable)
import Cardano.Protocol.TPraos.Rules.Overlay (OVERLAY, OverlayEnv (..))
import Cardano.Protocol.TPraos.Rules.Updn (UPDN, UpdnEnv (..), UpdnState (..))
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.State.Transition
import Data.Map.Strict (Map)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data PRTCL c

data PrtclState c
  = PrtclState
      !(Map (KeyHash 'BlockIssuer c) Word64)
      -- ^ Operation Certificate counters
      !Nonce
      -- ^ Evolving nonce
      !Nonce
      -- ^ Candidate nonce
  deriving (Generic, Show, Eq)

instance Crypto c => ToCBOR (PrtclState c)

instance Crypto c => EncCBOR (PrtclState c) where
  encCBOR (PrtclState m n1 n2) =
    mconcat
      [ encodeListLen 3
      , encCBOR m
      , encCBOR n1
      , encCBOR n2
      ]

instance Crypto c => FromCBOR (PrtclState c)

instance Crypto c => DecCBOR (PrtclState c) where
  decCBOR =
    decodeRecordNamed
      "PrtclState"
      (const 3)
      ( PrtclState
          <$> decCBOR
          <*> decCBOR
          <*> decCBOR
      )

instance Crypto c => NoThunks (PrtclState c)

data PrtclEnv c
  = PrtclEnv
      UnitInterval -- the decentralization paramater @d@ from the protocal parameters
      (PoolDistr c)
      (GenDelegs c)
      Nonce
  deriving (Generic)

instance NoThunks (PrtclEnv c)

data PrtclPredicateFailure c
  = OverlayFailure (PredicateFailure (OVERLAY c)) -- Subtransition Failures
  | UpdnFailure (PredicateFailure (UPDN c)) -- Subtransition Failures
  deriving (Generic)

data PrtclEvent c
  = UpdnEvent (Event (UPDN c)) -- Subtransition Failures
  | NoEvent Void

deriving instance
  (VRF.VRFAlgorithm (VRF c)) =>
  Show (PrtclPredicateFailure c)

deriving instance
  (VRF.VRFAlgorithm (VRF c)) =>
  Eq (PrtclPredicateFailure c)

instance
  ( Crypto c
  , DSignable c (OCertSignable c)
  , KESignable c (BHBody c)
  , VRFSignable c Seed
  ) =>
  STS (PRTCL c)
  where
  type
    State (PRTCL c) =
      PrtclState c

  type
    Signal (PRTCL c) =
      BHeader c

  type
    Environment (PRTCL c) =
      PrtclEnv c

  type BaseM (PRTCL c) = ShelleyBase
  type PredicateFailure (PRTCL c) = PrtclPredicateFailure c
  type Event (PRTCL c) = PrtclEvent c

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition ::
  forall c.
  ( Crypto c
  , DSignable c (OCertSignable c)
  , KESignable c (BHBody c)
  , VRFSignable c Seed
  ) =>
  TransitionRule (PRTCL c)
prtclTransition = do
  TRC
    ( PrtclEnv dval pd dms eta0
      , PrtclState cs etaV etaC
      , bh
      ) <-
    judgmentContext
  let bhb = bhbody bh
      slot = bheaderSlotNo bhb
      eta = bnonce bhb

  UpdnState etaV' etaC' <-
    trans @(UPDN c) $
      TRC
        ( UpdnEnv eta
        , UpdnState etaV etaC
        , slot
        )
  cs' <-
    trans @(OVERLAY c) $
      TRC (OverlayEnv dval pd dms eta0, cs, bh)

  pure $
    PrtclState
      cs'
      etaV'
      etaC'

instance (Crypto c) => NoThunks (PrtclPredicateFailure c)

instance
  ( Crypto c
  , DSignable c (OCertSignable c)
  , KESignable c (BHBody c)
  , VRFSignable c Seed
  ) =>
  Embed (OVERLAY c) (PRTCL c)
  where
  wrapFailed = OverlayFailure
  wrapEvent = NoEvent

instance
  ( Crypto c
  , DSignable c (OCertSignable c)
  , KESignable c (BHBody c)
  , VRFSignable c Seed
  ) =>
  Embed (UPDN c) (PRTCL c)
  where
  wrapFailed = UpdnFailure
  wrapEvent = UpdnEvent

data PrtlSeqFailure c
  = WrongSlotIntervalPrtclSeq
      SlotNo
      -- ^ Last slot number.
      SlotNo
      -- ^ Current slot number.
  | WrongBlockNoPrtclSeq
      (WithOrigin (LastAppliedBlock c))
      -- ^ Last applied block.
      BlockNo
      -- ^ Current block number.
  | WrongBlockSequencePrtclSeq
      (PrevHash c)
      -- ^ Last applied hash
      (PrevHash c)
      -- ^ Current block's previous hash
  deriving (Show, Eq, Generic)

instance Crypto c => NoThunks (PrtlSeqFailure c)

prtlSeqChecks ::
  (MonadError (PrtlSeqFailure c) m, Crypto c) =>
  WithOrigin (LastAppliedBlock c) ->
  BHeader c ->
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
