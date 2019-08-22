{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.Epoch where

import           Control.State.Transition
import           Data.Data (Data, Typeable)
import           Ledger.Core
import           Ledger.GlobalParams (slotsPerEpoch)
import           Ledger.Update


-- | Compute the epoch for the given _absolute_ slot and chain stability parameter.
sEpoch
  :: Slot
  -> BlockCount
  -> Epoch
sEpoch (Slot s) k = if k' > 0
                       then Epoch $ s `div` k'
                       else error ("sEpoch: bad `k` provided: " <> show k)
  where k' = slotsPerEpoch k

data EPOCH deriving (Data, Typeable)

instance STS EPOCH where
  type Environment EPOCH =
    ( Epoch
    , BlockCount -- Chain stability paramter; this is a global
                 -- constant in the formal specification, which we put
                 -- in this environment so that we can test with
                 -- different values of it.
    )
  type State EPOCH = UPIState

  type Signal EPOCH = Slot
  data PredicateFailure EPOCH =
    UPIECFailure (PredicateFailure UPIEC)
    deriving (Eq, Show, Data, Typeable)

  initialRules = []

  transitionRules =
    [ do
        TRC ((e_c, k), us, s) <- judgmentContext
        if sEpoch s k <= e_c
          then
            pure $! us
          else do
            us' <- trans @UPIEC $ TRC ((s, k), us, ())
            pure $! us'
    ]

instance Embed UPIEC EPOCH where
  wrapFailed = UPIECFailure
