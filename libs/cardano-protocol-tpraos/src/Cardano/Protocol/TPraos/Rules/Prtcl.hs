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
import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Binary.Plain (
  FromCBOR (..),
  ToCBOR (..),
  decodeRecordNamed,
  encodeListLen,
 )
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (
  DSignable,
  GenDelegs (..),
  KeyHash,
  KeyRole (..),
 )
import Cardano.Ledger.PoolDistr (PoolDistr)
import Cardano.Ledger.Slot (BlockNo, SlotNo)
import Cardano.Protocol.HeaderCrypto (HeaderCrypto, VRF)
import Cardano.Protocol.HeaderKeys (
  KESignable,
  VRFSignable,
 )
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

data PRTCL c hc

data PrtclState c
  = PrtclState
      !(Map (KeyHash 'BlockIssuer c) Word64)
      -- ^ Operation Certificate counters
      !Nonce
      -- ^ Evolving nonce
      !Nonce
      -- ^ Candidate nonce
  deriving (Generic, Show, Eq)

instance Crypto c => EncCBOR (PrtclState c)

instance Crypto c => ToCBOR (PrtclState c) where
  toCBOR (PrtclState m n1 n2) =
    mconcat
      [ encodeListLen 3
      , toCBOR m
      , toCBOR n1
      , toCBOR n2
      ]

instance Crypto c => DecCBOR (PrtclState c)

instance Crypto c => FromCBOR (PrtclState c) where
  fromCBOR =
    decodeRecordNamed
      "PrtclState"
      (const 3)
      ( PrtclState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
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

data PrtclPredicateFailure c hc
  = OverlayFailure (PredicateFailure (OVERLAY c hc)) -- Subtransition Failures
  | UpdnFailure (PredicateFailure (UPDN c)) -- Subtransition Failures
  deriving (Generic)

data PrtclEvent c
  = UpdnEvent (Event (UPDN c)) -- Subtransition Failures
  | NoEvent Void

deriving instance
  (VRF.VRFAlgorithm (VRF hc)) =>
  Show (PrtclPredicateFailure c hc)

deriving instance
  (VRF.VRFAlgorithm (VRF hc)) =>
  Eq (PrtclPredicateFailure c hc)

instance
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRFSignable hc Seed
  ) =>
  STS (PRTCL c hc)
  where
  type
    State (PRTCL c hc) =
      PrtclState c

  type
    Signal (PRTCL c hc) =
      BHeader c hc

  type
    Environment (PRTCL c hc) =
      PrtclEnv c

  type BaseM (PRTCL c hc) = ShelleyBase
  type PredicateFailure (PRTCL c hc) = PrtclPredicateFailure c hc
  type Event (PRTCL c hc) = PrtclEvent c

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition ::
  forall c hc.
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRFSignable hc Seed
  ) =>
  TransitionRule (PRTCL c hc)
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
    trans @(OVERLAY c hc) $
      TRC (OverlayEnv dval pd dms eta0, cs, bh)

  pure $
    PrtclState
      cs'
      etaV'
      etaC'

instance (Crypto c, HeaderCrypto hc) => NoThunks (PrtclPredicateFailure c hc)

instance
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRFSignable hc Seed
  ) =>
  Embed (OVERLAY c hc) (PRTCL c hc)
  where
  wrapFailed = OverlayFailure
  wrapEvent = NoEvent

instance
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRFSignable hc Seed
  ) =>
  Embed (UPDN c) (PRTCL c hc)
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
  (MonadError (PrtlSeqFailure c) m, Crypto c, HeaderCrypto hc) =>
  WithOrigin (LastAppliedBlock c) ->
  BHeader c hc ->
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
