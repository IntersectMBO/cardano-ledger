{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Protocol.TPraos.Rules.OCert
  ( OCERT,
    PredicateFailure,
    OCertEnv (..),
    OcertPredicateFailure (..),
  )
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys
import Cardano.Protocol.HeaderCrypto
import Cardano.Protocol.HeaderKeys
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

data OCERT crypto hcrypto

data OcertPredicateFailure crypto
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
      !(KeyHash 'BlockIssuer crypto) -- stake pool key hash
  deriving (Show, Eq, Generic)

instance NoThunks (OcertPredicateFailure crypto)

instance
  ( Crypto crypto,
    HeaderCrypto hcrypto,
    DSignable crypto (OCertSignable hcrypto),
    KESignable hcrypto (BHBody crypto hcrypto)
  ) =>
  STS (OCERT crypto hcrypto)
  where
  type
    State (OCERT crypto hcrypto) =
      Map (KeyHash 'BlockIssuer crypto) Word64
  type
    Signal (OCERT crypto hcrypto) =
      BHeader crypto hcrypto
  type Environment (OCERT crypto hcrypto) = OCertEnv crypto
  type BaseM (OCERT crypto hcrypto) = ShelleyBase
  type PredicateFailure (OCERT crypto hcrypto) = OcertPredicateFailure crypto

  initialRules = [pure Map.empty]
  transitionRules = [ocertTransition]

ocertTransition ::
  ( Crypto crypto,
    HeaderCrypto hcrypto,
    DSignable crypto (OCertSignable hcrypto),
    KESignable hcrypto (BHBody crypto hcrypto)
  ) =>
  TransitionRule (OCERT crypto hcrypto)
ocertTransition =
  judgmentContext >>= \(TRC (env, cs, BHeader bhb sigma)) -> do
    let OCert vk_hot n c0@(KESPeriod c0_) tau = bheaderOCert bhb
        vkcold = bheaderVk bhb
        hk = hashKey vkcold
        s = bheaderSlotNo bhb
    kp@(KESPeriod kp_) <- liftSTS $ kesPeriod s

    maxKESiterations <- liftSTS $ asks maxKESEvo

    c0 <= kp ?! KESBeforeStartOCERT c0 kp
    kp_ < c0_ + fromIntegral maxKESiterations
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
