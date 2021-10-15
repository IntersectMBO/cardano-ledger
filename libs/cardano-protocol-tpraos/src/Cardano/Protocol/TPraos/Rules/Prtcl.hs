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

module Cardano.Protocol.TPraos.Rules.Prtcl
  ( PRTCL,
    State,
    PrtclEnv (..),
    PrtclState (..),
    PrtclPredicateFailure (..),
    PredicateFailure,
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
import Cardano.Ledger.BaseTypes
  ( Nonce,
    Seed,
    ShelleyBase,
    UnitInterval,
  )
import Cardano.Ledger.Crypto (Crypto, VRF)
import Cardano.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    KESignable,
    KeyHash,
    KeyRole (..),
    VRFSignable,
  )
import Cardano.Ledger.PoolDistr (PoolDistr)
import Cardano.Ledger.Serialization (decodeRecordNamed)
import Cardano.Ledger.Slot (BlockNo, SlotNo)
import Cardano.Protocol.TPraos.BHeader
  ( BHBody (..),
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

data PRTCL crypto

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

instance Crypto crypto => NoThunks (PrtclState crypto)

data PrtclEnv crypto
  = PrtclEnv
      UnitInterval -- the decentralization paramater @d@ from the protocal parameters
      (PoolDistr crypto)
      (GenDelegs crypto)
      Nonce
  deriving (Generic)

instance NoThunks (PrtclEnv crypto)

data PrtclPredicateFailure crypto
  = OverlayFailure (PredicateFailure (OVERLAY crypto)) -- Subtransition Failures
  | UpdnFailure (PredicateFailure (UPDN crypto)) -- Subtransition Failures
  deriving (Generic)

data PrtclEvent crypto
  = UpdnEvent (Event (UPDN crypto)) -- Subtransition Failures
  | NoEvent Void

deriving instance
  (VRF.VRFAlgorithm (VRF crypto)) =>
  Show (PrtclPredicateFailure crypto)

deriving instance
  (VRF.VRFAlgorithm (VRF crypto)) =>
  Eq (PrtclPredicateFailure crypto)

instance
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  STS (PRTCL crypto)
  where
  type
    State (PRTCL crypto) =
      PrtclState crypto

  type
    Signal (PRTCL crypto) =
      BHeader crypto

  type
    Environment (PRTCL crypto) =
      PrtclEnv crypto

  type BaseM (PRTCL crypto) = ShelleyBase
  type PredicateFailure (PRTCL crypto) = PrtclPredicateFailure crypto
  type Event (PRTCL crypto) = PrtclEvent crypto

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  TransitionRule (PRTCL crypto)
prtclTransition = do
  TRC
    ( PrtclEnv dval pd dms eta0,
      PrtclState cs etaV etaC,
      bh
      ) <-
    judgmentContext
  let bhb = bhbody bh
      slot = bheaderSlotNo bhb
      eta = bnonce bhb

  UpdnState etaV' etaC' <-
    trans @(UPDN crypto) $
      TRC
        ( UpdnEnv eta,
          UpdnState etaV etaC,
          slot
        )
  cs' <-
    trans @(OVERLAY crypto) $
      TRC (OverlayEnv dval pd dms eta0, cs, bh)

  pure $
    PrtclState
      cs'
      etaV'
      etaC'

instance (Crypto crypto) => NoThunks (PrtclPredicateFailure crypto)

instance
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  Embed (OVERLAY crypto) (PRTCL crypto)
  where
  wrapFailed = OverlayFailure
  wrapEvent = NoEvent

instance
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  Embed (UPDN crypto) (PRTCL crypto)
  where
  wrapFailed = UpdnFailure
  wrapEvent = UpdnEvent

data PrtlSeqFailure crypto
  = WrongSlotIntervalPrtclSeq
      SlotNo
      -- ^ Last slot number.
      SlotNo
      -- ^ Current slot number.
  | WrongBlockNoPrtclSeq
      (WithOrigin (LastAppliedBlock crypto))
      -- ^ Last applied block.
      BlockNo
      -- ^ Current block number.
  | WrongBlockSequencePrtclSeq
      (PrevHash crypto)
      -- ^ Last applied hash
      (PrevHash crypto)
      -- ^ Current block's previous hash
  deriving (Show, Eq, Generic)

instance Crypto crypto => NoThunks (PrtlSeqFailure crypto)

prtlSeqChecks ::
  (MonadError (PrtlSeqFailure crypto) m, Crypto crypto) =>
  WithOrigin (LastAppliedBlock crypto) ->
  BHeader crypto ->
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
