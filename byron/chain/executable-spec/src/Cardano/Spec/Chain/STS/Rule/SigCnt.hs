{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.SigCnt where

import Control.Lens ((^.))
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

import Control.State.Transition

import Ledger.Core hiding ((|>))
import Ledger.GlobalParams (k)
import Ledger.Update

data SIGCNT

instance STS SIGCNT where
  type Environment SIGCNT
    = ( PParams
      , Bimap VKeyGenesis VKey
      )

  type State SIGCNT = Seq VKeyGenesis

  type Signal SIGCNT = VKey

  data PredicateFailure SIGCNT
    = TooManyIssuedBlocks VKeyGenesis -- The given genesis key issued too many blocks.
    | NotADelegate
    -- ^ The key signing the block is not a delegate of a genesis key.

    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, dms), sgs, vk) <- judgmentContext
        let t' = pps ^. bkSgnCntT
        case Bimap.lookupR vk dms of
          Just vkG -> do
            let sgs' = S.drop (S.length sgs + 1 - (fromIntegral . unBlockCount $ k)) (sgs |> vkG)
                nrSignedBks = fromIntegral (S.length (S.filter (==vkG) sgs'))
            nrSignedBks <= fromIntegral (unBlockCount k) * t' ?! TooManyIssuedBlocks vkG
            return $! sgs'
          Nothing -> do
            failBecause NotADelegate
            return sgs -- TODO: this is a quite inconvenient encoding for this transition system!
    ]
