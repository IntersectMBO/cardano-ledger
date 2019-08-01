{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

-- | Module containing the imports needed to test that a given abstract trace
-- passes the concrete validation. This is useful when debugging
-- counterexamples.
--
-- Usage from the ghci repl:
--
-- > import Hedgehog
-- > Hedgehog.check $ property $ passConcreteValidation trace0
--
-- Replace @trace0@ by the trace under analysis.
--
module Test.Cardano.Chain.Block.Model.Examples where

import GHC.Exts
import Cardano.Prelude hiding (trace, State)

import Control.State.Transition
import Control.State.Transition.Trace

import Ledger.Core
import Ledger.Delegation
import Ledger.Update
import Ledger.UTxO
import Cardano.Ledger.Spec.STS.UTXO

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain

-- | A trace example. When debugging conformance tests you can add such a
-- trace, and use it in the repl. __NOTE__: Do not commit such trace.
trace0 :: Trace CHAIN
trace0 = mkTrace traceEnv0 traceInitState0 traceTrans0
  where
    traceEnv0 :: Environment CHAIN
    traceEnv0 = panic "Add the trace environment here."

    traceInitState0 :: State CHAIN
    traceInitState0 = panic "Add the trace initial state here."

    traceTrans0 :: [(State CHAIN, Signal CHAIN)]
    traceTrans0 = panic "Add the trace transitions here."
