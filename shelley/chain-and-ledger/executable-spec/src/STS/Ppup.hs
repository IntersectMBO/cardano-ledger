{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Ppup
  ( PPUP
  , PPUPEnv(..)
  )
where

import           Data.Data (Data)
import           Data.Ix (inRange)
import qualified Data.Map.Strict as Map

import           BaseTypes
import           BlockChain
import           Keys
import           Ledger.Core (dom, (⊆))
import           PParams
import           Slot
import           Updates

import           Control.State.Transition
import           Numeric.Natural (Natural)

data PPUP crypto
  deriving Data

data PPUPEnv crypto
  = PPUPEnv Slot PParams (GenDelegs crypto)

instance STS (PPUP crypto) where
  type State (PPUP crypto) = PPUpdate crypto
  type Signal (PPUP crypto) = PPUpdate crypto
  type Environment (PPUP crypto) = PPUPEnv crypto
  data PredicateFailure (PPUP crypto)
    = NonGenesisUpdatePPUP String String
    -- ^ we use string representations of (dom pup) (dom delegs) to
    -- avoid the need for Data instances on the underlying crypto types
    | PPUpdateTooLatePPUP
    | PPUpdateEmpty
    | PPUpdateNonEmpty
    | PVCannotFollowPPUP
    deriving (Show, Eq, Data)

  initialRules = []

  transitionRules = [ppupTransitionEmpty, ppupTransitionNonEmpty]

pvCanFollow :: (Natural, Natural, Natural) -> Ppm -> Bool
pvCanFollow (mjp, mip, ap) (ProtocolVersion (mjn, mn, an))
  = (mjp, mip, ap) < (mjn, mn, an)
  && inRange (0,1) (mjn - mjp)
  && ((mjp == mjn) ==> (mip + 1 == mn))
  && ((mjp + 1 == mjn) ==> (mn == 0))
pvCanFollow _ _ = True

ppupTransitionEmpty :: TransitionRule (PPUP crypto)
ppupTransitionEmpty = do
  TRC (_, pupS, PPUpdate pup') <- judgmentContext

  Map.null pup' ?! PPUpdateNonEmpty

  pure pupS

ppupTransitionNonEmpty :: TransitionRule (PPUP crypto)
ppupTransitionNonEmpty = do
  TRC (PPUPEnv s pp (GenDelegs _genDelegs), pupS, pup@(PPUpdate pup')) <- judgmentContext

  pup' /= Map.empty ?! PPUpdateEmpty

  all (all (pvCanFollow (_protocolVersion pp))) pup' ?! PVCannotFollowPPUP

  (dom pup' ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (show (dom pup')) (show (dom _genDelegs))

  let Epoch slotEpoch = epochFromSlot (Slot 1)
  s
    <  (firstSlot (Epoch $ slotEpoch + 1) *- slotsPrior)
    ?! PPUpdateTooLatePPUP

  pure $ updatePPup pupS pup
