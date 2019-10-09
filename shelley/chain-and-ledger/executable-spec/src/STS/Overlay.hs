{-# LANGUAGE DeriveGeneric #-}
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
import           GHC.Generics (Generic)

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           OCert
import           PParams
import           Slot

import           STS.Ocert

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Prelude (NoUnexpectedThunks(..))
import           Control.State.Transition

data OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo

data OverlayEnv hashAlgo dsignAlgo kesAlgo vrfAlgo
  = OverlayEnv
      PParams
      (Map Slot (Maybe (GenKeyHash hashAlgo dsignAlgo)))
      Nonce
      (PoolDistr hashAlgo dsignAlgo vrfAlgo)
      (Dms hashAlgo dsignAlgo)
  deriving Generic

instance NoUnexpectedThunks (OverlayEnv hashAlgo dsignAlgo kesAlgo vrfAlgo)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => STS (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  type State (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = Map (KeyHash hashAlgo dsignAlgo) Natural

  type Signal (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo

  type Environment (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo) = OverlayEnv hashAlgo dsignAlgo kesAlgo vrfAlgo

  data PredicateFailure (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = NotPraosLeaderOVERLAY
    | NotActiveSlotOVERLAY
    | WrongGenesisColdKeyOVERLAY (KeyHash hashAlgo dsignAlgo) (KeyHash hashAlgo dsignAlgo)
    | NoGenesisStakingOVERLAY
    | OcertFailure (PredicateFailure (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules = []

  transitionRules = [overlayTransition]

overlayTransition
  :: forall hashAlgo dsignAlgo kesAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
     , VRFAlgorithm vrfAlgo
     , VRF.Signable vrfAlgo Seed
     )
  => TransitionRule (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo)
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

  trans @(OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo) $ TRC ((), cs, bh)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => Embed (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo) (OVERLAY hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = OcertFailure
