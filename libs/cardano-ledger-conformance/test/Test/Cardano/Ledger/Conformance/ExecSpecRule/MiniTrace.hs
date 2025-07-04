{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.MiniTrace where

import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.Governance (
  RatifySignal (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.Rules (GovSignal (..))
import Cardano.Ledger.Core
import Constrained.API
import Control.State.Transition.Extended (STS (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Proxy
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance
-- \| This is where most of the ExecSpecRule instances are defined
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway (
  nameCerts,
  nameDelegCert,
  nameEnact,
  nameEpoch,
  nameGovCert,
  namePoolCert,
  nameTxCert,
 )
import Test.Cardano.Ledger.Generic.Proof (Proof (..), goSTS)
import qualified Test.Cardano.Ledger.Generic.Proof as Proof

-- ====================================================================

-- | Generate either a list of signals, or a list of error messages
minitraceEither ::
  forall s e.
  ( ExecSpecRule s e
  , ExecState s e ~ State (EraRule s e)
  , ToExpr (Signal (EraRule s e))
  , ToExpr (State (EraRule s e))
  ) =>
  Int ->
  Gen (Either [String] [Signal (EraRule s e)])
minitraceEither n0 = do
  ctxt <- genExecContext @s @e
  env <- genFromSpec @(ExecEnvironment s e) (environmentSpec @s @e ctxt)
  let env2 :: Environment (EraRule s e)
      env2 = inject env
  !state0 <- genFromSpec @(ExecState s e) (stateSpec @s @e ctxt env)
  let go :: State (EraRule s e) -> Int -> Gen (Either [String] [Signal (EraRule s e)])
      go _ 0 = pure (Right [])
      go state n = do
        signal <- genFromSpec @(ExecSignal s e) (signalSpec @s @e ctxt env state)
        let signal2 :: Signal (EraRule s e)
            signal2 = inject signal
        goSTS @s @e @(Gen (Either [String] [Signal (EraRule s e)]))
          env2
          state
          signal2
          ( \x -> case x of
              Left ps ->
                pure
                  ( Left
                      ( [ "\nSIGNAL = " ++ show (toExpr signal2)
                        , "\nSTATE = " ++ show (toExpr state)
                        , "\nPredicateFailures"
                        ]
                          ++ map show (NE.toList ps)
                      )
                  )
              Right !state2 -> do
                ans <- go state2 (n - 1)
                case ans of
                  Left xs -> pure (Left xs)
                  Right more -> pure (Right (inject signal : more))
          )
  go state0 n0

minitrace ::
  forall s e.
  ( ExecSpecRule s e
  , ExecState s e ~ State (EraRule s e)
  , ToExpr (Signal (EraRule s e))
  , ToExpr (State (EraRule s e))
  ) =>
  Int ->
  Gen [Signal (EraRule s e)]
minitrace n0 = do
  ans <- minitraceEither @s @e n0
  case ans of
    Left zs -> pure $ error (unlines zs)
    Right zs -> pure zs

minitraceProp ::
  forall s e.
  ( ExecSpecRule s e
  , ExecState s e ~ State (EraRule s e)
  , ToExpr (Signal (EraRule s e))
  , ToExpr (State (EraRule s e))
  ) =>
  Int ->
  (Signal (EraRule s e) -> String) ->
  Gen Property
minitraceProp n0 namef = do
  ans <- minitraceEither @s @e n0
  case ans of
    Left zs -> pure $ counterexample (unlines zs) (property False)
    Right sigs -> pure $ classifyFirst namef sigs $ property True

-- =======================================================
-- Classifying what is in a trace requires a function that
-- lifts a Signal to a String
-- =======================================================

classifyMany :: (x -> String) -> [x] -> Property -> Property
classifyMany _ [] p = p
classifyMany f (x : xs) p = classifyMany f xs (classify True (f x) p)

classifyFirst :: (x -> String) -> [x] -> Property -> Property
classifyFirst _ [] p = p
classifyFirst f (x : _) p = classify True (f x) p

classifyFirst' :: (x -> Maybe String) -> [x] -> Property -> Property
classifyFirst' _ [] p = p
classifyFirst' f (x : _) p = maybe p (\s -> classify True s p) (f x)

nameRatify :: RatifySignal era -> String
nameRatify (RatifySignal xs) = show (length xs) ++ " GovActionStates"

nameGovSignal :: GovSignal Proof.ConwayEra -> String
nameGovSignal (GovSignal (VotingProcedures m) os cs) = show (Map.size m) ++ " " ++ show (OSet.size os) ++ " " ++ show (length cs)

nameAlonzoTx :: AlonzoTx era -> String
nameAlonzoTx (AlonzoTx _body _wits isV _auxdata) = show isV

-- | Run a minitrace for every instance of ExecRuleSpec
spec :: Spec
spec = do
  describe "50 MiniTrace tests with trace length of 50" $ do
    prop
      "POOL"
      (withMaxSuccess 50 (minitraceProp @"POOL" @ConwayEra 50 namePoolCert))
    prop
      "DELEG"
      (withMaxSuccess 50 (minitraceProp @"DELEG" @ConwayEra 50 nameDelegCert))
    prop
      "GOVCERT"
      ( withMaxSuccess
          50
          (minitraceProp @"GOVCERT" @ConwayEra 50 nameGovCert)
      )
    prop
      "CERT"
      (withMaxSuccess 50 (minitraceProp @"CERT" @ConwayEra 50 nameTxCert))
    prop
      "CERTS"
      ( withMaxSuccess
          50
          (minitraceProp @"CERTS" @ConwayEra 50 nameCerts)
      )
    prop
      "RATIFY"
      (withMaxSuccess 50 (minitraceProp @"RATIFY" @ConwayEra 50 nameRatify))
    -- prop "ENACT" (withMaxSuccess 50 (minitraceProp (ENACT Conway) (Proxy @ConwayFn) 50 nameEnact))
    -- These properties do not have working 'signalSpec' Specifications yet.
    xprop
      "GOV"
      (withMaxSuccess 50 (minitraceProp @"GOV" @ConwayEra 50 nameGovSignal))
    xprop
      "UTXO"
      (withMaxSuccess 50 (minitraceProp @"UTXO" @ConwayEra 50 nameAlonzoTx))
    xprop
      "EPOCH"
      (withMaxSuccess 50 (minitraceProp @"EPOCH" @ConwayEra 50 nameEpoch))
    xprop
      "NEWEPOCH"
      (withMaxSuccess 50 (minitraceProp @"NEWEPOCH" @ConwayEra 50 nameEpoch))
