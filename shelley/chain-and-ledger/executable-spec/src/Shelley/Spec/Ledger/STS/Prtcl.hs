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
  , PredicateFailure(..)
  , State
  , PrtclEnv(..)
  , PrtclState(..)
  )
where

import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.BlockChain
import           Shelley.Spec.Ledger.Delegation.Certificates
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.OCert
import           Shelley.Spec.Ledger.PParams
import           Shelley.Spec.Ledger.Slot

import           Shelley.Spec.Ledger.STS.Overlay
import           Shelley.Spec.Ledger.STS.Updn

import           Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR), decodeListLenOf,
                     encodeListLen)
import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition

data PRTCL crypto

data PrtclState crypto
  = PrtclState
      (Map (KeyHash crypto) Natural)
      (HashHeader crypto)
      SlotNo
      BlockNo
      Nonce -- ^ Current epoch nonce
      Nonce -- ^ Evolving nonce
      Nonce -- ^ Candidate nonce
      Nonce -- ^ Prev epoch hash nonce
  deriving (Generic, Show, Eq)

instance Crypto crypto => ToCBOR (PrtclState crypto) where
  toCBOR (PrtclState m hh sn bn n1 n2 n3 n4) = mconcat
    [ encodeListLen 8
    , toCBOR m
    , toCBOR hh
    , toCBOR sn
    , toCBOR bn
    , toCBOR n1
    , toCBOR n2
    , toCBOR n3
    , toCBOR n4
    ]

instance Crypto crypto => FromCBOR (PrtclState crypto) where
  fromCBOR = decodeListLenOf 8 >>
    PrtclState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance NoUnexpectedThunks (PrtclState crypto)

data PrtclEnv crypto
  = PrtclEnv
      PParams
      (Map SlotNo (Maybe (GenKeyHash crypto)))
      (PoolDistr crypto)
      (GenDelegs crypto)
      SlotNo
      Bool -- ^ New epoch marker
  deriving (Generic)

instance NoUnexpectedThunks (PrtclEnv crypto)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
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
    | WrongBlockNoPRTCL BlockNo BlockNo
    | WrongBlockSequencePRTCL
    | OverlayFailure (PredicateFailure (OVERLAY crypto))
    | UpdnFailure (PredicateFailure (UPDN crypto))
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
     , KESignable crypto (BHBody crypto)
     , VRF.Signable (VRF crypto) Seed
     )
  => TransitionRule (PRTCL crypto)
prtclTransition = do
  TRC ( PrtclEnv pp osched pd dms sNow ne
      , PrtclState cs h sL bL eta0 etaV etaC etaH
      , bh) <- judgmentContext
  let bhb  = bhbody bh
      bn = bheaderBlockNo bhb
      slot = bheaderSlotNo bhb
      eta  = fromNatural . VRF.certifiedNatural $ bheaderEta bhb
  sL < slot && slot <= sNow ?! WrongSlotIntervalPRTCL
  bL + 1 == bn ?! WrongBlockNoPRTCL bL bn
  h == bheaderPrev bhb ?! WrongBlockSequencePRTCL

  UpdnState eta0' etaV' etaC' etaH'
    <- trans @(UPDN crypto) $ TRC (UpdnEnv eta pp h ne, UpdnState eta0 etaV etaC etaH, slot)
  cs'
    <- trans @(OVERLAY crypto)
        $ TRC (OverlayEnv pp osched eta0' pd dms, cs, bh)

  pure $ PrtclState cs' (bhHash bh) slot bn eta0' etaV' etaC' etaH'

instance NoUnexpectedThunks (PredicateFailure (PRTCL crypto))

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (OVERLAY crypto) (PRTCL crypto)
 where
  wrapFailed = OverlayFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (UPDN crypto) (PRTCL crypto)
 where
  wrapFailed = UpdnFailure
