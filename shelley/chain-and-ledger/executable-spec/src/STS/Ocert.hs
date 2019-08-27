{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Ocert
  ( OCERT
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)

import           BlockChain
import           Keys
import           Ledger.Core ((⨃))
import           OCert

import           Control.State.Transition

data OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  )
  => STS (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  type State (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = Map (KeyHash hashAlgo dsignAlgo) Natural
  type Signal (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  type Environment (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo) = ()
  data PredicateFailure (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = KESBeforeStartOCERT
    | KESAfterEndOCERT
    | KESPeriodWrongOCERT
    | InvalidSignatureOCERT
    | InvalidKesSignatureOCERT
    | NoCounterForKeyHashOCERT
    deriving (Show, Eq)

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

ocertTransition
  :: ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
     )
  => TransitionRule (OCERT hashAlgo dsignAlgo kesAlgo vrfAlgo)
ocertTransition = do
  TRC (_, cs, BHeader bhb sigma) <- judgmentContext

  let OCert vk_hot vk_cold n c0@(KESPeriod c0_) tau = bheaderOCert bhb
      hk = hashKey vk_cold
      s = bheaderSlot bhb
      kp@(KESPeriod kp_) = kesPeriod s
      t = kp_ - c0_

  c0 <= kp ?! KESBeforeStartOCERT
  kp_ < c0_ + 90 ?! KESAfterEndOCERT

  verify vk_cold (vk_hot, n, c0) tau ?! InvalidSignatureOCERT
  verifyKES vk_hot bhb sigma t ?! InvalidKesSignatureOCERT

  case Map.lookup hk cs of
    Nothing -> do
      failBecause NoCounterForKeyHashOCERT
      pure cs
    Just m -> do
      m <= n ?! KESPeriodWrongOCERT
      pure $ cs ⨃ [(hk, n)]
