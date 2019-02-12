{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.Epoch where

import Data.Map.Strict (Map)

import Control.State.Transition
import Ledger.Core

import Cardano.Spec.Chain.STS.Block

data EPOCH

instance STS EPOCH where
  type Environment EPOCH = Map VKeyGenesis VKey
  type State EPOCH = Epoch
  type Signal EPOCH = Slot
  data PredicateFailure EPOCH = X
    deriving (Eq, Show)
  initialRules = []
  transitionRules =
    [ do
        TRC (_, _, s) <- judgmentContext
        return $! sEpoch s
    ]
