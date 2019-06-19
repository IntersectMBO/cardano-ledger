{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Ppup
  ( PPUP
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Numeric.Natural                ( Natural )

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           PParams
import           Slot
import           Updates

import           Control.State.Transition

data PPUP

instance STS PPUP where
  type State PPUP = PPUpdate
  type Signal PPUP = PPUpdate
  type Environment PPUP = (Slot, Dms)
  data PredicateFailure PPUP = NonGenesisUpdatePPUP
                             | PPUpdateTooLatePPUP
                             deriving (Show, Eq)
  initialRules = []

  transitionRules = [ppupTransition]

ppupTransition :: TransitionRule PPUP
ppupTransition = do
  TRC ((s, Dms dms), pupS@(PPUpdate pupS'), pup@(PPUpdate pup')) <-
    judgmentContext
  if Map.null pupS'
    then pure pupS
    else do
      (not $ Map.keysSet pup' `Set.isSubsetOf` Map.keysSet dms)
        ?! NonGenesisUpdatePPUP
      let Epoch slotEpoch = epochFromSlot (Slot 1)
      s
        <  (firstSlot (Epoch $ slotEpoch + 1) *- slotsPrior)
        ?! PPUpdateTooLatePPUP
      pure $ updatePPup pupS pup
