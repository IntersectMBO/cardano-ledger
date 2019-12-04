{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Prtcl
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

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           OCert
import           PParams
import           Slot

import           STS.Overlay
import           STS.Updn

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition

data PRTCL crypto

data PrtclState crypto
  = PrtclState
      (Map (KeyHash crypto) Natural)
      (HashHeader crypto)
      Slot
      Nonce -- ^ Current epoch nonce
      Nonce -- ^ Evolving nonce
      Nonce -- ^ Candidate nonce
      Nonce -- ^ Prev epoch hash nonce
  deriving (Generic, Show)

instance NoUnexpectedThunks (PrtclState crypto)

data PrtclEnv crypto
  = PrtclEnv
      PParams
      (Map Slot (Maybe (GenKeyHash crypto)))
      (PoolDistr crypto)
      (GenDelegs crypto)
      Slot
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

  data PredicateFailure (PRTCL crypto)
    = WrongSlotIntervalPRTCL
    | WrongBlockSequencePRTCL
    | OverlayFailure (PredicateFailure (OVERLAY crypto))
    | UpdnFailure (PredicateFailure (UPDN crypto))
    deriving (Show, Eq)

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
      , PrtclState cs h sL eta0 etaV etaC etaH
      , bh) <- judgmentContext
  let bhb  = bhbody bh
  let slot = bheaderSlot bhb
  let eta  = fromNatural . VRF.certifiedNatural $ bheaderEta bhb
  sL < slot && slot <= sNow ?! WrongSlotIntervalPRTCL
  h == bheaderPrev bhb ?! WrongBlockSequencePRTCL

  UpdnState eta0' etaV' etaC' etaH'
    <- trans @(UPDN crypto) $ TRC (UpdnEnv eta pp h ne, UpdnState eta0 etaV etaC etaH, slot)
  cs'
    <- trans @(OVERLAY crypto)
        $ TRC (OverlayEnv pp osched eta0' pd dms, cs, bh)

  pure $ PrtclState cs' (bhHash bh) slot eta0' etaV' etaC' etaH'

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
