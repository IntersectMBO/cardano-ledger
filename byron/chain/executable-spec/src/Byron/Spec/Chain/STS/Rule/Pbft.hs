{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Byron.Spec.Chain.STS.Rule.Pbft where

import Byron.Spec.Chain.STS.Block
import Byron.Spec.Chain.STS.Rule.SigCnt
import Byron.Spec.Ledger.Core
import Byron.Spec.Ledger.Update
import Control.State.Transition
import Data.Bimap (Bimap)
import Data.Data (Data, Typeable)
import Data.Sequence (Seq)
import Lens.Micro ((^.))

data PBFT deriving (Data, Typeable)

data PbftPredicateFailure
  = SlotNotAfterLastBlock Slot Slot
  | SlotInTheFuture Slot Slot
  | PrevHashNotMatching Hash Hash
  | InvalidHeaderSignature VKey (Sig Hash)
  | SigCountFailure (PredicateFailure SIGCNT)
  deriving (Eq, Show, Data, Typeable)

instance STS PBFT where
  type
    Environment PBFT =
      ( PParams,
        Bimap VKeyGenesis VKey,
        Slot,
        Slot,
        BlockCount -- Chain stability parameter
      )

  type State PBFT = (Hash, Seq VKeyGenesis)

  type Signal PBFT = BlockHeader

  type PredicateFailure PBFT = PbftPredicateFailure

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, ds, sLast, sNow, k), (h, sgs), bh) <- judgmentContext
        let vkd = bh ^. bhIssuer :: VKey
            s = bh ^. bhSlot :: Slot
        s > sLast ?! SlotNotAfterLastBlock s sLast
        s <= sNow ?! SlotInTheFuture s sNow
        (bh ^. bhPrevHash) == h ?! PrevHashNotMatching (bh ^. bhPrevHash) h
        verify vkd (bhToSign bh) (bh ^. bhSig) ?! InvalidHeaderSignature vkd (bh ^. bhSig)
        sgs' <- trans @SIGCNT $ TRC ((pps, ds, k), sgs, vkd)
        pure $! (bhHash bh, sgs')
    ]

instance Embed SIGCNT PBFT where
  wrapFailed = SigCountFailure
