{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Ocert
  ( OCERT
  , OCertEnv(..)
  )
where

import           Byron.Spec.Ledger.Core ((⨃))
import           Cardano.Prelude (NoUnexpectedThunks, asks)
import           Control.State.Transition
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)
import           Shelley.Spec.Ledger.BaseTypes
import           Shelley.Spec.Ledger.BlockChain
import           Shelley.Spec.Ledger.Crypto
import           Shelley.Spec.Ledger.Keys
import           Shelley.Spec.Ledger.OCert

data OCERT crypto

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  )
  => STS (OCERT crypto)
 where
  type State (OCERT crypto)
    = Map (KeyHash crypto) Natural
  type Signal (OCERT crypto)
    = BHeader crypto
  type Environment (OCERT crypto) = OCertEnv crypto
  type BaseM (OCERT crypto) = ShelleyBase
  data PredicateFailure (OCERT crypto)
    = KESBeforeStartOCERT
    | KESAfterEndOCERT
    | KESPeriodWrongOCERT
    | InvalidSignatureOCERT
    | InvalidKesSignatureOCERT
    | NoCounterForKeyHashOCERT
    deriving (Show, Eq, Generic)

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

instance NoUnexpectedThunks (PredicateFailure (OCERT crypto))

ocertTransition
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
     , KESignable crypto (BHBody crypto)
     )
  => TransitionRule (OCERT crypto)
ocertTransition = judgmentContext >>= \(TRC (env, cs, BHeader bhb sigma)) -> do
  let OCert vk_hot n c0@(KESPeriod c0_) tau = bheaderOCert bhb
      vkey = bheaderVk bhb
      hk = hashKey vkey
      s = bheaderSlotNo bhb
  kp@(KESPeriod kp_) <- liftSTS $ kesPeriod s

  maxKESiterations <- liftSTS $ asks maxKESEvo

  c0 <= kp ?! KESBeforeStartOCERT
  kp_ < c0_ + (fromIntegral maxKESiterations) ?! KESAfterEndOCERT

  let t = if kp_ >= c0_ then kp_ - c0_ else 0 -- this is required to prevent an
                                              -- arithmetic underflow, in the
                                              -- case of kp_ < c0_ we get the
                                              -- above `KESBeforeStartOCERT`
                                              -- predicate failure in the
                                              -- transition.

  verify vkey (vk_hot, n, c0) tau ?! InvalidSignatureOCERT
  verifyKES vk_hot bhb sigma t ?! InvalidKesSignatureOCERT

  case currentIssueNo env cs hk of
    Nothing -> do
      failBecause NoCounterForKeyHashOCERT
      pure cs
    Just m -> do
      m <= n ?! KESPeriodWrongOCERT
      pure $ cs ⨃ [(hk, n)]
