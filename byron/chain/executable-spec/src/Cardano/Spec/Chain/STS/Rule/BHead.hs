{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Spec.Chain.STS.Rule.BHead where

import Control.Lens ((^.), _1)
import Data.Bimap (Bimap)

import Control.State.Transition
import Ledger.Core
import Ledger.Update

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Epoch

data BHEAD

instance STS BHEAD where
  type Environment BHEAD
    = ( Bimap VKeyGenesis VKey
      , Slot
      )
  type State BHEAD = UPIState

  type Signal BHEAD = BlockHeader

  data PredicateFailure BHEAD
    = HashesDontMatch -- TODO: Add fields so that users know the two hashes that don't match
    | HeaderSizeTooBig -- TODO: Add more information here as well.
    | SlotDidNotIncrease
    -- ^ The block header slot number did not increase w.r.t the last seen slot
    | SlotInTheFuture
    -- ^ The block header slot number is greater than the current slot (As
    -- specified in the environment).
    | EpochFailure (PredicateFailure EPOCH)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((_, sLast), us, bh) <- judgmentContext
        us' <- trans @EPOCH $ TRC (sEpoch sLast, us, bh ^. bhSlot)
        let sMax = snd (us' ^. _1) ^. maxHdrSz
        bHeaderSize bh <= sMax ?! HeaderSizeTooBig
        return $! us'
    ]

instance Embed EPOCH BHEAD where
  wrapFailed = EpochFailure
