{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Ocert
  ( OCERT
  )
where

import qualified Data.Map.Strict               as Map
import           Numeric.Natural                ( Natural )

import           BlockChain
import           Keys
import           OCert

import           Control.State.Transition

data OCERT

instance STS OCERT where
  type State OCERT = Map.Map HashKey Natural
  type Signal OCERT = BHeader
  type Environment OCERT = ()
  data PredicateFailure OCERT = KESBeforeStartOCERT
                            | KESAfterEndOCERT
                            | KESPeriodWrongOCERT
                            | InvalidSignatureOCERT
                            | InvalidKesSignatureOCERT
                            | NoCounterForHashKeyOCERT
                                deriving (Show, Eq)
  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

ocertTransition :: TransitionRule OCERT
ocertTransition = do
  TRC (_, cs, BHeader bhb sigma) <- judgmentContext
  let OCert vk_hot vk_cold n c0@(KESPeriod c0') tau = bheaderOCert bhb
  let hk = hashKey vk_cold
  let s  = bheaderSlot bhb
  not (verify vk_cold (vk_hot, n, c0) tau) ?! InvalidSignatureOCERT
  let kp@(KESPeriod kp') = kesPeriod s
  c0 > kp ?! KESBeforeStartOCERT
  kp' >= c0' + 90 ?! KESAfterEndOCERT
  let t = kp' - c0'
  not (verifyKES vk_hot bhb sigma t) ?! InvalidKesSignatureOCERT
  let hkEntry = Map.lookup hk cs
  case hkEntry of
    Nothing -> do
      failBecause NoCounterForHashKeyOCERT
      pure cs
    Just m -> do
      m > n ?! KESPeriodWrongOCERT
      pure $ Map.insert hk n cs
