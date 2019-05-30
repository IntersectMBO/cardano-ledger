{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Spec.Chain.STS.Rule.Epoch where

-- import Control.Lens ((^.), _2)
import Control.State.Transition
import Data.Bimap (Bimap)
import Ledger.Core
import Ledger.Update


-- | Compute the epoch for the given _absolute_ slot
sEpoch :: Slot -> Epoch
sEpoch (Slot s) = Epoch $ s `div` 21600


data EPOCH

instance STS EPOCH where
  type Environment EPOCH = Epoch
  type State EPOCH = UPIState

  type Signal EPOCH = Slot
  data PredicateFailure EPOCH =
    UPIECFailure (PredicateFailure UPIEC)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC (e_c, _, s) <- judgmentContext
        case e_c >= sEpoch s of
          True  -> onOrAfterCurrentEpoch
          False -> beforeCurrentEpoch
    ]
   where
    beforeCurrentEpoch :: TransitionRule EPOCH
    beforeCurrentEpoch = do
      TRC (_, us, s) <- judgmentContext
      us' <- trans @UPIEC $ TRC (s, us, ())
      return $! us'

    onOrAfterCurrentEpoch :: TransitionRule EPOCH
    onOrAfterCurrentEpoch = do
      TRC (_, us, _) <- judgmentContext
      return $! us

instance Embed UPIEC EPOCH where
  wrapFailed = UPIECFailure
