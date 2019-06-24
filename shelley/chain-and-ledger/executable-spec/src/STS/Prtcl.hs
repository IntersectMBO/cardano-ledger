{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Prtcl
  ( PRTCL
  )
where

import qualified Data.Map.Strict               as Map
import           Numeric.Natural                ( Natural )

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           PParams
import           Slot

import           STS.Overlay
import           STS.Updn

import           Control.State.Transition

data PRTCL

instance STS PRTCL where
  type State PRTCL = (Map.Map HashKey Natural, HashHeader, Slot, Seed, Seed)
  type Signal PRTCL = BHeader
  type Environment PRTCL =
    ( -- OverlayEnvironment
      ( PParams
      , Map.Map Slot (Maybe VKeyGenesis)
      , Seed
      , PoolDistr
      , Dms)
    , Slot)
  data PredicateFailure PRTCL = WrongSlotIntervalPRTCL
                              | WrongBlockSequencePRTCL
                              | OverlayFailure (PredicateFailure OVERLAY)
                              | UpdnFailure (PredicateFailure UPDN)
                                   deriving (Show, Eq)
  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition :: TransitionRule PRTCL
prtclTransition = do
  TRC ((oe, sNow), (cs, h, sL, etaV, etaC), bh) <- judgmentContext
  let bhb  = bhbody bh
  let slot = bheaderSlot bhb
  let eta  = bheaderEta bhb
  sL < slot && slot <= sNow ?! WrongSlotIntervalPRTCL
  h == bheaderPrev bhb ?! WrongBlockSequencePRTCL

  cs'            <- trans @OVERLAY $ TRC (oe, cs, bh)
  (etaV', etaC') <- trans @UPDN $ TRC (eta, (etaV, etaC), slot)

  pure (cs', bhHash bh, slot, etaV', etaC')

instance Embed OVERLAY PRTCL where
  wrapFailed = OverlayFailure

instance Embed UPDN PRTCL where
  wrapFailed = UpdnFailure
