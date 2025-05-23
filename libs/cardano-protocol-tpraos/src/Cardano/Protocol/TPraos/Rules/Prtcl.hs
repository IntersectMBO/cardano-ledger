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
) where

import qualified Cardano.Crypto.KES as KES
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
import Cardano.Ledger.Keys (GenDelegs (..), KeyHash, KeyRole (..))
import Cardano.Ledger.Slot (BlockNo, SlotNo)
import Cardano.Ledger.State (PoolDistr)
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader (..),
  LastAppliedBlock (..),
  PrevHash,
  bhbody,
  bnonce,
  lastAppliedHash,
 )
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

data PrtclState
  = PrtclState
      -- | Operation Certificate counters
      !(Map (KeyHash 'BlockIssuer) Word64)
      -- | Evolving nonce
      !Nonce
      -- | Candidate nonce
      !Nonce
  deriving (Generic, Show, Eq)

instance EncCBOR PrtclState

instance ToCBOR PrtclState where
  toCBOR (PrtclState m n1 n2) =
    mconcat
      [ encodeListLen 3
      , toCBOR m
      , toCBOR n1
      , toCBOR n2
      ]

instance DecCBOR PrtclState

instance FromCBOR PrtclState where
  fromCBOR =
    decodeRecordNamed
      "PrtclState"
      (const 3)
      ( PrtclState
          <$> fromCBOR
          <*> fromCBOR
          <*> fromCBOR
      )

instance NoThunks PrtclState

data PrtclEnv
  = PrtclEnv
      UnitInterval -- the decentralization paramater @d@ from the protocal parameters
      PoolDistr
      GenDelegs
      Nonce
  deriving (Generic)

instance NoThunks PrtclEnv

data PrtclPredicateFailure c
  = OverlayFailure (PredicateFailure (OVERLAY c)) -- Subtransition Failures
  | UpdnFailure (PredicateFailure (UPDN c)) -- Subtransition Failures
  deriving (Generic)

data PrtclEvent c
  = UpdnEvent (Event (UPDN c)) -- Subtransition Failures
  | NoEvent Void

deriving instance
  VRF.VRFAlgorithm (VRF c) =>
  Show (PrtclPredicateFailure c)

deriving instance
  VRF.VRFAlgorithm (VRF c) =>
  Eq (PrtclPredicateFailure c)

instance
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  STS (PRTCL c)
  where
  type State (PRTCL c) = PrtclState

  type Signal (PRTCL c) = BHeader c

  type Environment (PRTCL c) = PrtclEnv

  type BaseM (PRTCL c) = ShelleyBase
  type PredicateFailure (PRTCL c) = PrtclPredicateFailure c
  type Event (PRTCL c) = PrtclEvent c

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition ::
  forall c.
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
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

instance Crypto c => NoThunks (PrtclPredicateFailure c)

instance
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  Embed (OVERLAY c) (PRTCL c)
  where
  wrapFailed = OverlayFailure
  wrapEvent = NoEvent

instance
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  Embed (UPDN c) (PRTCL c)
  where
  wrapFailed = UpdnFailure
  wrapEvent = UpdnEvent

data PrtlSeqFailure
  = WrongSlotIntervalPrtclSeq
      -- | Last slot number.
      SlotNo
      -- | Current slot number.
      SlotNo
  | WrongBlockNoPrtclSeq
      -- | Last applied block.
      (WithOrigin LastAppliedBlock)
      -- | Current block number.
      BlockNo
  | WrongBlockSequencePrtclSeq
      -- | Last applied hash
      PrevHash
      -- | Current block's previous hash
      PrevHash
  deriving (Show, Eq, Generic)

instance NoThunks PrtlSeqFailure

prtlSeqChecks ::
  (MonadError PrtlSeqFailure m, Crypto c) =>
  WithOrigin LastAppliedBlock ->
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
