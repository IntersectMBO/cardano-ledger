{-# LANGUAGE TypeFamilies #-}

module Cardano.Spec.Chain.STS.Rule.SigCnt where

import Control.Lens ((^.), to)
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S

import Control.State.Transition

import Ledger.Core hiding ((|>))
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
    | NonInjectiveDelegationMap
    -- ^ Delegation rules should restrict the delegation map to be injective.
    --
    -- Should not be needed once
    -- https://github.com/input-output-hk/cardano-chain/issues/257 gets
    -- resolved.

    deriving (Eq, Show)

  initialRules = []

  transitionRules =
    [ do
        TRC ((pps, dms), sgs, vk) <- judgmentContext
        let k = pps ^. stableAfter . to unBlockCount . to fromIntegral
            t = pps ^. bkSgnCntT
        case Bimap.lookupR vk dms of
        -- Currently we do not restrict the number of delegators to a given key
        -- in the delegation rules. See: https://github.com/input-output-hk/cardano-chain/issues/257
        --
        -- Even we implement the restriction in the delegation rules, we might
        -- still want to check this (unless the types forbid such situation).
          Just vkG -> do
            let sgs' = S.drop (S.length sgs + 1 - k) (sgs |> vkG)
                nrSignedBks = fromIntegral (S.length (S.filter (==vkG) sgs'))
            nrSignedBks <= fromIntegral k * t ?! TooManyIssuedBlocks vkG
            return $! sgs'
          Nothing -> do
            failBecause NotADelegate
            return sgs -- TODO: this is a quite inconvenient encoding for this transition system!
    ]
