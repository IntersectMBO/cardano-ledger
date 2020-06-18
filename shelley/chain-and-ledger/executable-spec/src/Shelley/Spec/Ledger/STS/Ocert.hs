{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Ocert
  ( OCERT,
    PredicateFailure (..),
    OCertEnv (..),
  )
where

import Cardano.Prelude (NoUnexpectedThunks, asks)
import Control.State.Transition
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Core (addpair)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.OCert

data OCERT crypto

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto)
  ) =>
  STS (OCERT crypto)
  where
  type
    State (OCERT crypto) =
      Map (KeyHash 'BlockIssuer crypto) Natural
  type
    Signal (OCERT crypto) =
      BHeader crypto
  type Environment (OCERT crypto) = OCertEnv crypto
  type BaseM (OCERT crypto) = ShelleyBase
  data PredicateFailure (OCERT crypto)
    = KESBeforeStartOCERT
        !KESPeriod -- OCert Start KES Period
        !KESPeriod -- Current KES Period
    | KESAfterEndOCERT
        !KESPeriod -- Current KES Period
        !KESPeriod -- OCert Start KES Period
        !Word64 -- Max KES Key Evolutions
    | KESPeriodWrongOCERT
        !Natural -- last KES counter used
        !Natural -- current KES counter
    | InvalidSignatureOCERT -- TODO use whole OCert
        !Natural -- OCert counter
        !KESPeriod -- OCert KES period
    | InvalidKesSignatureOCERT
        !Word -- current KES Period
        !Word -- KES start period
        !Word -- expected KES evolutions
        !String -- error message given by Consensus Layer
    | NoCounterForKeyHashOCERT
        !(KeyHash 'BlockIssuer crypto) -- stake pool key hash
    deriving (Show, Eq, Generic)

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

instance NoUnexpectedThunks (PredicateFailure (OCERT crypto))

ocertTransition ::
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto)
  ) =>
  TransitionRule (OCERT crypto)
ocertTransition = judgmentContext >>= \(TRC (env, cs, BHeader bhb sigma)) -> do
  let OCert vk_hot n c0@(KESPeriod c0_) tau = bheaderOCert bhb
      vkey = bheaderVk bhb
      hk = hashKey vkey
      s = bheaderSlotNo bhb
  kp@(KESPeriod kp_) <- liftSTS $ kesPeriod s

  maxKESiterations <- liftSTS $ asks maxKESEvo

  c0 <= kp ?! KESBeforeStartOCERT c0 kp
  kp_ < c0_ + (fromIntegral maxKESiterations)
    ?! KESAfterEndOCERT kp c0 maxKESiterations

  let t = if kp_ >= c0_ then kp_ - c0_ else 0 -- this is required to prevent an
  -- arithmetic underflow, in the
  -- case of kp_ < c0_ we get the
  -- above `KESBeforeStartOCERT`
  -- predicate failure in the
  -- transition.
  verifySignedDSIGN vkey (vk_hot, n, c0) tau ?! InvalidSignatureOCERT n c0
  verifySignedKES () vk_hot t bhb sigma ?!: InvalidKesSignatureOCERT kp_ c0_ t

  case currentIssueNo env cs hk of
    Nothing -> do
      failBecause $ NoCounterForKeyHashOCERT hk
      pure cs
    Just m -> do
      m <= n ?! KESPeriodWrongOCERT m n
      pure $ addpair hk n cs
