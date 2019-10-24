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
import           Keys
import           OCert
import           Slot

import           STS.Overlay
import           STS.Updn

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Control.State.Transition

data PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo

data PrtclState hashAlgo dsignAlgo kesAlgo vrfAlgo
  = PrtclState
      (Map (KeyHash hashAlgo dsignAlgo) Natural)
      (HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo)
      Slot
      Nonce
      Nonce
  deriving (Generic, Show)

instance NoUnexpectedThunks (PrtclState hashAlgo dsignAlgo kesAlgo vrfAlgo)

data PrtclEnv hashAlgo dsignAlgo kesAlgo vrfAlgo
  = PrtclEnv (OverlayEnv hashAlgo dsignAlgo kesAlgo vrfAlgo) Slot
  deriving (Generic)

instance NoUnexpectedThunks (PrtclEnv hashAlgo dsignAlgo kesAlgo vrfAlgo)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => STS (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  type State (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = PrtclState hashAlgo dsignAlgo kesAlgo vrfAlgo

  type Signal (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo

  type Environment (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = PrtclEnv hashAlgo dsignAlgo kesAlgo vrfAlgo

  data PredicateFailure (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = WrongSlotIntervalPRTCL
    | WrongBlockSequencePRTCL
    | OverlayFailure (PredicateFailure (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo))
    | UpdnFailure (PredicateFailure UPDN)
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition
  :: forall hashAlgo dsignAlgo kesAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
     , VRFAlgorithm vrfAlgo
     , VRF.Signable vrfAlgo Seed
     )
  => TransitionRule (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
prtclTransition = do
  TRC (PrtclEnv oe sNow, PrtclState cs h sL etaV etaC, bh) <- judgmentContext
  let bhb  = bhbody bh
  let slot = bheaderSlot bhb
  let eta  = fromNatural . VRF.certifiedNatural $ bheaderEta bhb
  sL < slot && slot <= sNow ?! WrongSlotIntervalPRTCL
  h == bheaderPrev bhb ?! WrongBlockSequencePRTCL

  cs'            <- trans @(OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo) $ TRC (oe, cs, bh)
  UpdnState etaV' etaC' <- trans @UPDN $ TRC (eta, UpdnState etaV etaC, slot)

  pure $ PrtclState cs' (bhHash bh) slot etaV' etaC'

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => Embed (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo) (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = OverlayFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => Embed UPDN (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = UpdnFailure
