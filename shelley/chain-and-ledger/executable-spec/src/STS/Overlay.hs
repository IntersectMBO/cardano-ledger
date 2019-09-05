{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Overlay
  ( OVERLAY
  , OverlayEnv(..)
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           OCert
import           PParams
import           Slot

import           STS.Ocert

import           Control.State.Transition

data OVERLAY hashAlgo dsignAlgo kesAlgo

data OverlayEnv hashAlgo dsignAlgo kesAlgo
  = OverlayEnv
      PParams
      (Map Slot (Maybe (GenKeyHash hashAlgo dsignAlgo)))
      Seed
      (PoolDistr hashAlgo dsignAlgo)
      (Dms hashAlgo dsignAlgo)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => STS (OVERLAY hashAlgo dsignAlgo kesAlgo)
 where
  type State (OVERLAY hashAlgo dsignAlgo kesAlgo)
    = Map (KeyHash hashAlgo dsignAlgo) Natural

  type Signal (OVERLAY hashAlgo dsignAlgo kesAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo

  type Environment (OVERLAY hashAlgo dsignAlgo kesAlgo) = OverlayEnv hashAlgo dsignAlgo kesAlgo

  data PredicateFailure (OVERLAY hashAlgo dsignAlgo kesAlgo)
    = NotPraosLeaderOVERLAY
    | NotActiveSlotOVERLAY
    | WrongGenesisColdKeyOVERLAY (KeyHash hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
    | NoGenesisStakingOVERLAY
    | OcertFailure (PredicateFailure (OCERT hashAlgo dsignAlgo kesAlgo))
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [overlayTransition]

overlayTransition
  :: forall hashAlgo dsignAlgo kesAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
     )
  => TransitionRule (OVERLAY hashAlgo dsignAlgo kesAlgo)
overlayTransition = do
  TRC ( OverlayEnv pp osched eta0 pd (Dms dms)
      , cs
      , bh@(BHeader bhb _)) <- judgmentContext
  let vk = bvkcold bhb
      vkh = hashKey vk

  case Map.lookup (bheaderSlot bhb) osched of
    Nothing ->
      vrfChecks eta0 pd (_activeSlotCoeff pp) bhb ?! NotPraosLeaderOVERLAY
    Just Nothing ->
      failBecause NotActiveSlotOVERLAY
    Just (Just gkey) ->
      case Map.lookup gkey dms of
        Nothing ->
          failBecause NoGenesisStakingOVERLAY
        Just dmsKey ->
          vkh == dmsKey ?! WrongGenesisColdKeyOVERLAY vkh dmsKey

  trans @(OCERT hashAlgo dsignAlgo kesAlgo) $ TRC ((), cs, bh)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed (OCERT hashAlgo dsignAlgo kesAlgo) (OVERLAY hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = OcertFailure
