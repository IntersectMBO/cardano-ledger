{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Ocert
  ( OCERT,
    PredicateFailure,
    OCertEnv (..),
    OcertPredicateFailure (..),
  )
where

import Cardano.Ledger.Era
import Cardano.Prelude (NoUnexpectedThunks, asks)
import Control.Iterate.SetAlgebra (eval, singleton, (⨃))
import Control.State.Transition
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.OCert

data OCERT era

data OcertPredicateFailure era
  = KESBeforeStartOCERT
      !KESPeriod -- OCert Start KES Period
      !KESPeriod -- Current KES Period
  | KESAfterEndOCERT
      !KESPeriod -- Current KES Period
      !KESPeriod -- OCert Start KES Period
      !Word64 -- Max KES Key Evolutions
  | CounterTooSmallOCERT
      !Word64 -- last KES counter used
      !Word64 -- current KES counter
  | InvalidSignatureOCERT -- TODO use whole OCert
      !Word64 -- OCert counter
      !KESPeriod -- OCert KES period
  | InvalidKesSignatureOCERT
      !Word -- current KES Period
      !Word -- KES start period
      !Word -- expected KES evolutions
      !String -- error message given by Consensus Layer
  | NoCounterForKeyHashOCERT
      !(KeyHash 'BlockIssuer era) -- stake pool key hash
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (OcertPredicateFailure era)

instance
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era)
  ) =>
  STS (OCERT era)
  where
  type
    State (OCERT era) =
      Map (KeyHash 'BlockIssuer era) Word64
  type
    Signal (OCERT era) =
      BHeader era
  type Environment (OCERT era) = OCertEnv era
  type BaseM (OCERT era) = ShelleyBase
  type PredicateFailure (OCERT era) = OcertPredicateFailure era

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

ocertTransition ::
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era)
  ) =>
  TransitionRule (OCERT era)
ocertTransition =
  judgmentContext >>= \(TRC (env, cs, BHeader bhb sigma)) -> do
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
    verifySignedDSIGN vkey (ocertToSignable $ bheaderOCert bhb) tau ?! InvalidSignatureOCERT n c0
    verifySignedKES () vk_hot t bhb sigma ?!: InvalidKesSignatureOCERT kp_ c0_ t

    case currentIssueNo env cs hk of
      Nothing -> do
        failBecause $ NoCounterForKeyHashOCERT hk
        pure cs
      Just m -> do
        m <= n ?! CounterTooSmallOCERT m n
        pure (eval (cs ⨃ (singleton hk n))) --  pure $ addpair hk n cs
