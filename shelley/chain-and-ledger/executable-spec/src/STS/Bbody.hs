{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Bbody
  ( BBODY
  ) where

import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)

import           BlockChain
import           EpochBoundary
import           Keys
import           LedgerState
import           PParams

import           Control.State.Transition

import           STS.Ledgers

data BBODY

instance STS BBODY where
  type State BBODY = (LedgerState, BlocksMade)
  type Signal BBODY = Block
  type Environment BBODY = PParams
  data PredicateFailure BBODY = BodySizeTooLargeBBODY
                            | InvalidTxsSignatureBBODY
                            | LedgersFailure (PredicateFailure LEDGERS)
                                deriving (Show, Eq)
  initialRules = [pure (emptyLedgerState, BlocksMade Map.empty)]
  transitionRules = [bbodyTransition]

bbodyTransition :: TransitionRule BBODY
bbodyTransition = do
  TRC (pp, (ls, BlocksMade b), (Block (BHeader bhb _) txs)) <-
    judgmentContext
  let vk = bheaderVk bhb
  let sigma = bheaderBlockSignature bhb
  not (verify vk txs sigma) ?! InvalidTxsSignatureBBODY
  bBodySize txs < (fromIntegral $ _maxBBSize pp) ?! BodySizeTooLargeBBODY
  let hk = hashKey vk
  let n = fromMaybe 0 (Map.lookup hk b)
  ls' <- trans @LEDGERS $ TRC ((bheaderSlot bhb, pp), ls, txs)
  pure (ls', BlocksMade $ Map.insert hk (n + 1) b)

instance Embed LEDGERS BBODY where
  wrapFailed = LedgersFailure
