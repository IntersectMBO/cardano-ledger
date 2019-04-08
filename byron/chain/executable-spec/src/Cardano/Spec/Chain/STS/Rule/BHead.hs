{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Spec.Chain.STS.Rule.BHead where

import Control.Lens ((^.))
import Data.Map.Strict (Map)
import Data.Sequence (Seq)

import Control.State.Transition
import Ledger.Core
import Ledger.Update

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Epoch
import Cardano.Spec.Chain.STS.Rule.SigCnt

data BHEAD

instance STS BHEAD where
  type Environment BHEAD
    = ( Slot
      , Map VKeyGenesis VKey
      )
  type State BHEAD
    = ( Epoch
      , Slot
      , Hash
      , Seq VKeyGenesis
      , PParams
      )

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
    | SigCntFailure (PredicateFailure SIGCNT)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ( (sNow, dms)
            , (eLast, sLast, hLast, sgs, us)
            , bh ) <- judgmentContext
        -- Check header size
        let sMax = us ^. maxHdrSz
        bHeaderSize bh <= sMax ?! HeaderSizeTooBig
        -- Check that the previous hash matches
        bh ^. bhPrevHash == hLast ?! HashesDontMatch
        -- Check sanity of current slot
        let sNext = bh ^. bhSlot
        sLast < sNext ?! SlotDidNotIncrease
        sNext <= sNow ?! SlotInTheFuture
        -- Perform an epoch transition
        eNext <-  trans @EPOCH $ TRC (us ^. bkSlotsPerEpoch, eLast, sNext)
        -- Perform a signature count transition
        sgs' <- trans @SIGCNT $ TRC ((us, dms), sgs, bh ^. bhIssuer)
        return $! ( eNext
                  , sNext
                  , hashHeader bh -- the same as bhToSign bh
                  , sgs'
                  , us
                  )
    ]

instance Embed EPOCH BHEAD where
  wrapFailed = EpochFailure

instance Embed SIGCNT BHEAD where
  wrapFailed = SigCntFailure
