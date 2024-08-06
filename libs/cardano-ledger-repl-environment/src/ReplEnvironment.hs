{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | This module imports a lot of useful definitions to make it easier to work
-- with `cabal repl`. Feel free to add additional imports here if you deem
-- something useful for debugging in the REPL.
--
-- To use this module, just run `cabal repl cardano-ledger-repl-environment` and
-- the REPL should load this module automatically.
module ReplEnvironment where

import Cardano.Ledger.Api
import Cardano.Ledger.Binary
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Crypto
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.Val
import Test.Cardano.Ledger.Api.DebugTools

import Control.Monad

import qualified Data.ByteString as BS
import Data.Default
import Data.Foldable
import Data.List
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Maybe.Strict
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set

import Lens.Micro

import System.IO

import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (crecGovActionMapL)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Core.Rational

import Test.QuickCheck
