{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Ppup
  ( PPUP
  , PPUPEnv(..)
  )
where

import qualified Data.Map.Strict as Map
import           Data.Set (Set)

import           BaseTypes
import           BlockChain
import           Keys
import           Ledger.Core (dom, (⊆), (⨃))
import           PParams
import           Slot
import           Updates

import           Control.State.Transition
import           Data.Ix (inRange)
import           Numeric.Natural (Natural)

data PPUP crypto

data PPUPEnv crypto
  = PPUPEnv Slot PParams (GenDelegs crypto)

instance STS (PPUP crypto) where
  type State (PPUP crypto) = PPUpdate crypto
  type Signal (PPUP crypto) = PPUpdate crypto
  type Environment (PPUP crypto) = PPUPEnv crypto
  data PredicateFailure (PPUP crypto)
    = NonGenesisUpdatePPUP (Set (GenKeyHash crypto)) (Set (GenKeyHash crypto))
    | PPUpdateTooLatePPUP
    | PPUpdateEmpty
    | PPUpdateNonEmpty
    | PVCannotFollowPPUP
    deriving (Show, Eq)

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
  TRC (PPUPEnv s pp (GenDelegs _genDelegs), PPUpdate pupS, PPUpdate pup) <- judgmentContext

  not (Map.null pup) ?! PPUpdateEmpty

  all (all (pvCanFollow (_protocolVersion pp)) . ppmSet) pup ?! PVCannotFollowPPUP

  (dom pup ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (dom pup) (dom _genDelegs)

  let Epoch e = epochFromSlot s
  s < firstSlot (Epoch $ e + 1) *- slotsPrior ?! PPUpdateTooLatePPUP

  pure $ PPUpdate (pupS ⨃  Map.toList pup)
