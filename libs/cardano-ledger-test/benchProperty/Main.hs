{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
-- Embed instances for (AlonzoEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
nix-shell --arg config "{ haskellNix.profiling = true; }"
-}
-- Second set up the cabal.project.local (in the root of the cardano-ledger directory) as follows.
{-
ignore-project: False
profiling: True
profiling-detail: all-functions
package plutus-core
  ghc-options: -fexternal-interpreter
-}
-- In the cardano-ledger-test directory (where the cardano-ledger-test.cabal file resides)
-- This cabal file defines the benchProperty benchmark. Now build with profiling enabled
--    cabal build benchProperty --enable-profiling
-- Now run the build with the -- +RTS -p   flags
--    cabal run benchProperty -- +RTS -p
--
-- When you are done be sure an reset the  cabal.project.local, and rebuild things
-- without profiling enabled. One way to do this is
-- Reset the cabal.project.local file
--  cabal configure
-- Rebuild everything in the current project
--  cabal build

-- | This benchmark file is for profiling Property tests. It appears as a benchmark
--     but we do not use any of the criterion stuff. We just run main, which is profiled.
-- First be sure and start the nix-shell with profiling enabled
module Main where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY, AlonzoUTXOW)
import Cardano.Ledger.Shelley.Rules (
  ShelleyLEDGER,
  ShelleyLedgerEvent (UtxowEvent),
  ShelleyLedgerPredFailure (UtxowFailure),
 )
import Control.State.Transition.Extended (Embed (..))
import Test.Cardano.Ledger.Alonzo.AlonzoEraGen ()
import Test.Cardano.Ledger.Alonzo.EraMapping ()
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN, ChainEvent (..), TestChainPredicateFailure (..))
import Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces (relevantCasesAreCovered)
import qualified Test.Tasty as T

-- ===============================================================

instance Embed (AlonzoBBODY (AlonzoEra TestCrypto)) (CHAIN (AlonzoEra TestCrypto)) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

instance Embed (AlonzoUTXOW (AlonzoEra TestCrypto)) (ShelleyLEDGER (AlonzoEra TestCrypto)) where
  wrapFailed = UtxowFailure
  wrapEvent = UtxowEvent

profileCover :: IO ()
profileCover =
  T.defaultMain $
    relevantCasesAreCovered @(AlonzoEra TestCrypto) 1

main :: IO ()
main = profileCover
