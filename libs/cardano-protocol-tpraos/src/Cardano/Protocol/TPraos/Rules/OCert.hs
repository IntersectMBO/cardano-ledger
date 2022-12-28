{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Protocol.TPraos.Rules.OCert (
  OCERT,
  PredicateFailure,
  OCertEnv (..),
  OcertPredicateFailure (..),
)
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys
import Cardano.Protocol.TPraos.BHeader
import Cardano.Protocol.TPraos.OCert
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (eval, singleton, (⨃))
import Control.State.Transition
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data OCERT c

data OcertPredicateFailure c
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
  | InvalidSignatureOCERT
      !Word64 -- OCert counter
      !KESPeriod -- OCert KES period
  | InvalidKesSignatureOCERT
      !Word -- current KES Period
      !Word -- KES start period
      !Word -- expected KES evolutions
      !String -- error message given by Consensus Layer
  | NoCounterForKeyHashOCERT
      !(KeyHash 'BlockIssuer c) -- stake pool key hash
  deriving (Show, Eq, Generic)

instance NoThunks (OcertPredicateFailure c)

instance
  ( Crypto c
  , DSignable c (OCertSignable c)
  , KESignable c (BHBody c)
  ) =>
  STS (OCERT c)
  where
  type
    State (OCERT c) =
      Map (KeyHash 'BlockIssuer c) Word64
  type
    Signal (OCERT c) =
      BHeader c
  type Environment (OCERT c) = OCertEnv c
  type BaseM (OCERT c) = ShelleyBase
  type PredicateFailure (OCERT c) = OcertPredicateFailure c

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

ocertTransition ::
  ( Crypto c
  , DSignable c (OCertSignable c)
  , KESignable c (BHBody c)
  ) =>
  TransitionRule (OCERT c)
ocertTransition =
  judgmentContext >>= \(TRC (env, cs, BHeader bhb sigma)) -> do
    let OCert vk_hot n c0@(KESPeriod c0_) tau = bheaderOCert bhb
        vkcold = bheaderVk bhb
        hk = hashKey vkcold
        s = bheaderSlotNo bhb
    kp@(KESPeriod kp_) <- liftSTS $ kesPeriod s

    maxKESiterations <- liftSTS $ asks maxKESEvo

    c0 <= kp ?! KESBeforeStartOCERT c0 kp
    kp_
      < c0_
      + fromIntegral maxKESiterations
      ?! KESAfterEndOCERT kp c0 maxKESiterations

    let t = if kp_ >= c0_ then kp_ - c0_ else 0 -- this is required to prevent an
    -- arithmetic underflow, in the
    -- case of kp_ < c0_ we get the
    -- above `KESBeforeStartOCERT`
    -- predicate failure in the
    -- transition.
    verifySignedDSIGN vkcold (ocertToSignable $ bheaderOCert bhb) tau ?! InvalidSignatureOCERT n c0
    verifySignedKES () vk_hot t bhb sigma ?!: InvalidKesSignatureOCERT kp_ c0_ t

    case currentIssueNo env cs hk of
      Nothing -> do
        failBecause $ NoCounterForKeyHashOCERT hk
        pure cs
      Just m -> do
        m <= n ?! CounterTooSmallOCERT m n
        pure (eval (cs ⨃ singleton hk n))
