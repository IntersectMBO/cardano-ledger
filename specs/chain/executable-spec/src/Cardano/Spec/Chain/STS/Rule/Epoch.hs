{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.Epoch where

import Control.State.Transition
import Ledger.Core

import Cardano.Spec.Chain.STS.Block

data EPOCH

instance STS EPOCH where
  type Environment EPOCH = SlotCount
  type State EPOCH = Epoch
  type Signal EPOCH = Slot
  data PredicateFailure EPOCH = X
    deriving (Eq, Show)
  initialRules = []
  transitionRules =
    [ do
        TRC (spe, _, s) <- judgmentContext
        return $! sEpoch s spe
    ]
