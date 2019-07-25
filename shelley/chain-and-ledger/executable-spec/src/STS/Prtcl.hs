{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Prtcl
  ( PRTCL
  )
where

import qualified Data.Map.Strict as Map
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

import           Control.State.Transition

data PRTCL hashAlgo dsignAlgo kesAlgo

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => STS (PRTCL hashAlgo dsignAlgo kesAlgo)
 where
  type State (PRTCL hashAlgo dsignAlgo kesAlgo)
    = ( Map.Map (KeyHash hashAlgo dsignAlgo) Natural
      , Maybe (HashHeader hashAlgo dsignAlgo kesAlgo)
      , Slot
      , Seed
      , Seed
      )

  type Signal (PRTCL hashAlgo dsignAlgo kesAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo

  type Environment (PRTCL hashAlgo dsignAlgo kesAlgo) =
    ( -- OverlayEnvironment
      ( PParams
      , Map.Map Slot (Maybe (VKeyGenesis dsignAlgo))
      , Seed
      , PoolDistr hashAlgo dsignAlgo
      , Dms dsignAlgo
      )
    , Slot
    )
  data PredicateFailure (PRTCL hashAlgo dsignAlgo kesAlgo)
    = WrongSlotIntervalPRTCL
    | WrongBlockSequencePRTCL
    | OverlayFailure (PredicateFailure (OVERLAY hashAlgo dsignAlgo kesAlgo))
    | UpdnFailure (PredicateFailure UPDN)
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition
  :: forall hashAlgo dsignAlgo kesAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
     )
  => TransitionRule (PRTCL hashAlgo dsignAlgo kesAlgo)
prtclTransition = do
  TRC ((oe, sNow), (cs, h, sL, etaV, etaC), bh) <- judgmentContext
  let bhb  = bhbody bh
  let slot = bheaderSlot bhb
  let eta  = bheaderEta bhb
  sL < slot && slot <= sNow ?! WrongSlotIntervalPRTCL
  h == bheaderPrev bhb ?! WrongBlockSequencePRTCL

  cs'            <- trans @(OVERLAY hashAlgo dsignAlgo kesAlgo) $ TRC (oe, cs, bh)
  (etaV', etaC') <- trans @UPDN $ TRC (eta, (etaV, etaC), slot)

  pure (cs', Just $ bhHash bh, slot, etaV', etaC')

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed (OVERLAY hashAlgo dsignAlgo kesAlgo) (PRTCL hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = OverlayFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed UPDN (PRTCL hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = UpdnFailure
