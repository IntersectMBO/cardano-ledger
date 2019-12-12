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
import           BaseTypes
import           BlockChain
import           Keys
import           Ledger.Core ((⨃))
import           OCert

import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition

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
    deriving (Show, Eq)

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

ocertTransition
  :: ( Crypto crypto
     , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
     , KESignable crypto (BHBody crypto)
     )
  => TransitionRule (OCERT crypto)
ocertTransition = do
  TRC (env, cs, BHeader bhb sigma) <- judgmentContext

  let OCert vk_hot vk_cold n c0@(KESPeriod c0_) tau = bheaderOCert bhb
      hk = hashKey vk_cold
      s = bheaderSlotNo bhb
  kp@(KESPeriod kp_) <- liftSTS $ kesPeriod s

  let t = kp_ - c0_

  c0 <= kp ?! KESBeforeStartOCERT
  kp_ < c0_ + 90 ?! KESAfterEndOCERT

  verify vk_cold (vk_hot, n, c0) tau ?! InvalidSignatureOCERT
  verifyKES vk_hot bhb sigma t ?! InvalidKesSignatureOCERT

  case currentIssueNo env cs hk of
    Nothing -> do
      failBecause NoCounterForKeyHashOCERT
      pure cs
    Just m -> do
      m <= n ?! KESPeriodWrongOCERT
      pure $ cs ⨃ [(hk, n)]
