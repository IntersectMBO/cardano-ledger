{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Overlay
  ( OVERLAY
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

import           STS.Ocert

import           Control.State.Transition

data OVERLAY

instance STS OVERLAY where
  type State OVERLAY = Map.Map HashKey Natural
  type Signal OVERLAY = BHeader
  type Environment OVERLAY =
    ( PParams
    , Map.Map Slot (Maybe VKeyGenesis)
    , Seed
    , PoolDistr
    , Dms)
  data PredicateFailure OVERLAY = NotPraosLeaderOVERLAY
                                | NotActiveSlotOVERLAY
                                | WrongGenesisColdKeyOVERLAY
                                | NoGenesisStakingOVERLAY
                                | OcertFailure (PredicateFailure OCERT)
                                   deriving (Show, Eq)
  initialRules = []

  transitionRules = [overlayTransition]

overlayTransition :: TransitionRule OVERLAY
overlayTransition = do
  TRC ((pp, osched, eta0, pd, Dms dms), cs, bh@(BHeader bhb _)) <-
    judgmentContext
  let gkey'' = Map.lookup (bheaderSlot bhb) osched
  let vk     = bvkcold bhb
  case gkey'' of
    Nothing -> do
      vrfChecks eta0 pd (_activeSlotCoeff pp) bhb ?! NotPraosLeaderOVERLAY
      cs' <- trans @OCERT $ TRC ((), cs, bh)
      pure cs'
    Just gkey' -> do
      case gkey' of
        Nothing   -> failBecause NotActiveSlotOVERLAY
        Just gkey -> do
          let dmsKey' = Map.lookup gkey dms
          case dmsKey' of
            Nothing     -> failBecause NoGenesisStakingOVERLAY
            Just dmsKey -> vk == dmsKey ?! WrongGenesisColdKeyOVERLAY
      cs' <- trans @OCERT $ TRC ((), cs, bh)
      pure cs'

instance Embed OCERT OVERLAY where
  wrapFailed = OcertFailure
