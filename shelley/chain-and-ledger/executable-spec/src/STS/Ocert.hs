{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Ocert
  ( OCERT
  ) where

import qualified Data.Map.Strict          as Map

import           BlockChain
import           Keys
import           LedgerState
import           OCert

import           Control.State.Transition

data OCERT

instance STS OCERT where
  type State OCERT = PState
  type Signal OCERT = BHBody
  type Environment OCERT = ()
  data PredicateFailure OCERT = KESBeforeStartOCERT
                            | KESAfterEndOCERT
                            | KESPeriodWrongOCERT
                            | InvalidSignatureOCERT
                            | NoCounterForHashKeyOCERT
                                deriving (Show, Eq)
  initialRules = [pure emptyPState]
  transitionRules = [ocertTransition]

ocertTransition :: TransitionRule OCERT
ocertTransition = do
  TRC (_, ps, bhb) <- judgmentContext
  let OCert vk_hot n c0@(KESPeriod c0') sigma = bheaderOCert bhb
  let vk_cold = bheaderVk bhb
  let hk = hashKey vk_cold
  not (verify vk_cold (vk_hot, n, c0) sigma) ?! InvalidSignatureOCERT
  let kp@(KESPeriod kp') = kesPeriod (bheaderSlot bhb)
  c0 > kp ?! KESBeforeStartOCERT
  kp' >= c0' + 90 ?! KESAfterEndOCERT
  let hkEntry = Map.lookup hk (_cCounters ps)
  case hkEntry of
    Nothing -> do
      failBecause NoCounterForHashKeyOCERT
      pure emptyPState
    Just m -> do
      m > n ?! KESPeriodWrongOCERT
      pure $ ps {_cCounters = Map.insert hk n (_cCounters ps)}
