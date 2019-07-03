{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.Pbft where

import           Control.Lens ((^.))
import           Data.Bimap (Bimap)
import           Data.Sequence (Seq)

import           Control.State.Transition

import           Ledger.Core
import           Ledger.Update

import           Cardano.Spec.Chain.STS.Block
import           Cardano.Spec.Chain.STS.Rule.SigCnt

data PBFT

instance STS PBFT where
  type Environment PBFT =
    ( PParams
    , Bimap VKeyGenesis VKey
    , Slot
    , Slot
    , BlockCount -- Chain stability parameter
    )

  type State PBFT = (Hash, Seq VKeyGenesis)

  type Signal PBFT = BlockHeader

  data PredicateFailure PBFT
    = SlotNotAfterLastBlock Slot Slot
    | SlotInTheFuture Slot Slot
    | PrevHashNotMatching Hash Hash
    | InvalidHeaderSignature VKey (Sig Hash)
    | SigCountFailure (PredicateFailure SIGCNT)
    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, ds, sLast, sNow, k), (h, sgs), bh) <- judgmentContext
        let
          vkd = bh ^. bhIssuer :: VKey
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
