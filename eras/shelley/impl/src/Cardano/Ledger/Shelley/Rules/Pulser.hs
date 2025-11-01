{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Pulser (
  ShelleyPULSER,
) where

instance STS (ShelleyPULSER era) where
  type State (ShelleyNEWPP era) = PulserState era
  type Signal (ShelleyNEWPP era) = ()
  type Environment (ShelleyNEWPP era) = ()
  type BaseM (ShelleyNEWPP era) = ShelleyBase
  type PredicateFailure (ShelleyNEWPP era) = Void
  transitionRules = [shelleyPulserTransition]

shelleyPulserTransition :: TransitionRule (ShelleyPULSER era)
shelleyPulserTransition = do
  TRC ((), pulserState, ()) <- judgmentContext
  
  pure pulserState
