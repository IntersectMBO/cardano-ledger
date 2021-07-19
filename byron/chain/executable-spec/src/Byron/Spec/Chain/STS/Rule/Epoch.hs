{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Byron.Spec.Chain.STS.Rule.Epoch where

import Byron.Spec.Ledger.Core
import Byron.Spec.Ledger.GlobalParams (slotsPerEpoch)
import Byron.Spec.Ledger.Update
import Control.State.Transition
import Data.Data (Data, Typeable)
import GHC.Stack (HasCallStack)

-- | Compute the epoch for the given _absolute_ slot and chain stability parameter.
sEpoch ::
  HasCallStack =>
  Slot ->
  BlockCount ->
  Epoch
sEpoch (Slot s) k =
  if k' > 0
    then Epoch $ s `div` k'
    else error ("sEpoch: bad `k` provided: " <> show k)
  where
    k' = slotsPerEpoch k

data EPOCH deriving (Data, Typeable)

data EpochPredicateFailure
  = UPIECFailure (PredicateFailure UPIEC)
  deriving (Eq, Show, Data, Typeable)

instance STS EPOCH where
  type
    Environment EPOCH =
      ( Epoch,
        BlockCount -- Chain stability paramter; this is a global
        -- constant in the formal specification, which we put
        -- in this environment so that we can test with
        -- different values of it.
      )
  type State EPOCH = UPIState
  type Signal EPOCH = Slot
  type PredicateFailure EPOCH = EpochPredicateFailure

  initialRules = []

  transitionRules =
    [ do
        TRC ((e_c, k), us, s) <- judgmentContext
        if sEpoch s k <= e_c
          then pure $! us
          else do
            us' <- trans @UPIEC $ TRC ((sEpoch s k, k), us, ())
            pure $! us'
    ]

instance Embed UPIEC EPOCH where
  wrapFailed = UPIECFailure
