{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Prtcl
  ( PRTCL
  , State
  , PrtclEnv(..)
  , PrtclState(..)
  , PredicateFailure(..)
  )
where

import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.BaseTypes (Nonce, Seed, ShelleyBase)
import           Shelley.Spec.Ledger.BlockChain (BHBody (..), BHeader (..), LastAppliedBlock (..),
                     bhHash, bhbody, lastAppliedHash)
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import           Shelley.Spec.Ledger.Keys (VRFSignable, KeyRole(..), GenDelegs (..), KESignable, KeyHash, DSignable, VerKeyKES,
                     fromNatural)
import           Shelley.Spec.Ledger.LedgerState (OBftSlot)
import           Shelley.Spec.Ledger.OCert (KESPeriod)
import           Shelley.Spec.Ledger.PParams (PParams)
import           Shelley.Spec.Ledger.Slot (BlockNo, SlotNo)

import           Shelley.Spec.Ledger.STS.Overlay (OVERLAY, OverlayEnv (..))
import           Shelley.Spec.Ledger.STS.Updn (UPDN, UpdnEnv (..), UpdnState (..))

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLenOf,
                     encodeListLen)
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Cardano.Slotting.Slot (WithOrigin (..), withOriginFromMaybe, withOriginToMaybe)
import           Control.State.Transition
import           Shelley.Spec.Ledger.Crypto (Crypto)

data PRTCL crypto

data PrtclState crypto
  = PrtclState
      !(Map (KeyHash 'BlockIssuer crypto) Natural)
      !(WithOrigin (LastAppliedBlock crypto))
      !Nonce -- ^ Current epoch nonce
      !Nonce -- ^ Evolving nonce
      !Nonce -- ^ Candidate nonce
      !Nonce -- ^ Prev epoch hash nonce
  deriving (Generic, Show, Eq)

instance Crypto crypto => ToCBOR (PrtclState crypto) where
  toCBOR (PrtclState m lab n1 n2 n3 n4) = mconcat
    [ encodeListLen 6
    , toCBOR m
    , toCBOR $ withOriginToMaybe lab
    , toCBOR n1
    , toCBOR n2
    , toCBOR n3
    , toCBOR n4
    ]

instance Crypto crypto => FromCBOR (PrtclState crypto) where
  fromCBOR = decodeListLenOf 6 >>
    PrtclState
      <$> fromCBOR
      <*> (withOriginFromMaybe <$> fromCBOR)
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance Crypto crypto => NoUnexpectedThunks (PrtclState crypto)

data PrtclEnv crypto
  = PrtclEnv
      PParams
      (Map SlotNo (OBftSlot crypto))
      (PoolDistr crypto)
      (GenDelegs crypto)
      SlotNo
      Bool -- ^ New epoch marker
  deriving (Generic)

instance NoUnexpectedThunks (PrtclEnv crypto)

instance
  ( Crypto crypto
  , DSignable crypto (VerKeyKES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRFSignable crypto Seed
  )
  => STS (PRTCL crypto)
 where
  type State (PRTCL crypto)
    = PrtclState crypto

  type Signal (PRTCL crypto)
    = BHeader crypto

  type Environment (PRTCL crypto)
    = PrtclEnv crypto

  type BaseM (PRTCL crypto) = ShelleyBase

  data PredicateFailure (PRTCL crypto)
    = WrongSlotIntervalPRTCL
    | WrongBlockNoPRTCL (WithOrigin (LastAppliedBlock crypto)) BlockNo
    | WrongBlockSequencePRTCL
    | OverlayFailure (PredicateFailure (OVERLAY crypto))
    | UpdnFailure (PredicateFailure (UPDN crypto))
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition
  :: forall crypto
   . ( Crypto crypto
     , DSignable crypto (VerKeyKES crypto, Natural, KESPeriod)
     , KESignable crypto (BHBody crypto)
     , VRFSignable crypto Seed
     )
  => TransitionRule (PRTCL crypto)
prtclTransition = do
  TRC ( PrtclEnv pp osched pd dms sNow ne
      , PrtclState cs lab eta0 etaV etaC etaH
      , bh) <- judgmentContext
  let bhb  = bhbody bh
      bn = bheaderBlockNo bhb
      slot = bheaderSlotNo bhb
      eta  = fromNatural . VRF.certifiedNatural $ bheaderEta bhb
      ph = lastAppliedHash lab

  case lab of
    Origin -> pure ()
    At (LastAppliedBlock bL sL _) -> do
      sL < slot && slot <= sNow ?! WrongSlotIntervalPRTCL
      bL + 1 == bn ?! WrongBlockNoPRTCL lab bn
  ph == bheaderPrev bhb ?! WrongBlockSequencePRTCL

  UpdnState eta0' etaV' etaC' etaH'
    <- trans @(UPDN crypto) $ TRC ( UpdnEnv eta pp ph ne
                                  , UpdnState eta0 etaV etaC etaH
                                  , slot)
  cs'
    <- trans @(OVERLAY crypto)
        $ TRC (OverlayEnv osched eta0' pd dms, cs, bh)

  pure $ PrtclState
           cs'
           (At $ LastAppliedBlock
                   bn
                   slot
                   (bhHash bh)
           )
           eta0'
           etaV'
           etaC'
           etaH'

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (PRTCL crypto))

instance
  ( Crypto crypto
  , DSignable crypto (VerKeyKES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRFSignable crypto Seed
  )
  => Embed (OVERLAY crypto) (PRTCL crypto)
 where
  wrapFailed = OverlayFailure

instance
  ( Crypto crypto
  , DSignable crypto (VerKeyKES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRFSignable crypto Seed
  )
  => Embed (UPDN crypto) (PRTCL crypto)
 where
  wrapFailed = UpdnFailure
