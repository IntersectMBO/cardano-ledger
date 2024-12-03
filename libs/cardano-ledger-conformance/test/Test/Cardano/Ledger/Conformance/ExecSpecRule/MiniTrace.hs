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
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  RatifySignal (..),
  VotingProcedures (..),
 )
import Cardano.Ledger.Conway.Rules (GovSignal (..))
import Cardano.Ledger.Core
import Constrained hiding (inject)
import Control.State.Transition.Extended (STS (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Proxy
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Constrained.Conway.Instances (ConwayFn)
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..))
import Test.Cardano.Ledger.Generic.Proof (Proof (..), WitRule (..), goSTS)
import qualified Test.Cardano.Ledger.Generic.Proof as Proof

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

-- ====================================================================

-- | Generate either a list of signals, or a list of error messages
minitraceEither ::
  forall fn s e.
  ( ExecSpecRule fn s e
  , ExecState fn s e ~ State (EraRule s e)
  , PrettyA (Signal (EraRule s e))
  , PrettyA (State (EraRule s e))
  ) =>
  WitRule s e ->
  Proxy fn ->
  Int ->
  Gen (Either [String] [Signal (EraRule s e)])
minitraceEither witrule Proxy n0 = do
  ctxt <- genExecContext @fn @s @e
  env <- genFromSpec @fn @(ExecEnvironment fn s e) (environmentSpec @fn @s @e ctxt)
  let env2 :: Environment (EraRule s e)
      env2 = inject env
  !state0 <- genFromSpec @fn @(ExecState fn s e) (stateSpec @fn @s @e ctxt env)
  let go :: State (EraRule s e) -> Int -> Gen (Either [String] [Signal (EraRule s e)])
      go _ 0 = pure (Right [])
      go state n = do
        signal <- genFromSpec @fn @(ExecSignal fn s e) (signalSpec @fn @s @e ctxt env state)
        let signal2 :: Signal (EraRule s e)
            signal2 = inject signal
        goSTS @s @e @(Gen (Either [String] [Signal (EraRule s e)]))
          witrule
          env2
          state
          signal2
          ( \x -> case x of
              Left ps ->
                pure
                  ( Left
                      ( [ "\nSIGNAL = " ++ show (prettyA signal2)
                        , "\nSTATE = " ++ show (prettyA state)
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
  forall fn s e.
  ( ExecSpecRule fn s e
  , ExecState fn s e ~ State (EraRule s e)
  , PrettyA (Signal (EraRule s e))
  , PrettyA (State (EraRule s e))
  ) =>
  WitRule s e ->
  Proxy fn ->
  Int ->
  Gen [Signal (EraRule s e)]
minitrace witrule Proxy n0 = do
  ans <- minitraceEither @fn @s @e witrule Proxy n0
  case ans of
    Left zs -> pure $ error (unlines zs)
    Right zs -> pure zs

minitraceProp ::
  forall s e.
  ( ExecSpecRule ConwayFn s e
  , ExecState ConwayFn s e ~ State (EraRule s e)
  , PrettyA (Signal (EraRule s e))
  , PrettyA (State (EraRule s e))
  ) =>
  WitRule s e ->
  Int ->
  (Signal (EraRule s e) -> String) ->
  Gen Property
minitraceProp witrule n0 namef = do
  ans <- minitraceEither @ConwayFn @s @e witrule (Proxy @ConwayFn) n0
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

nameGovSignal :: GovSignal Conway -> String
nameGovSignal (GovSignal (VotingProcedures m) os cs) = show (Map.size m) ++ " " ++ show (OSet.size os) ++ " " ++ show (length cs)

nameAlonzoTx :: AlonzoTx era -> String
nameAlonzoTx (AlonzoTx _body _wits isV _auxdata) = show isV

-- | Run a minitrace for every instance of ExecRuleSpec
spec :: Spec
spec = do
  describe "50 MiniTrace tests with trace length of 50" $ do
    prop
      "POOL"
      (withMaxSuccess 50 (minitraceProp (POOL Conway) 50 namePoolCert))
    prop
      "DELEG"
      (withMaxSuccess 50 (minitraceProp (DELEG Conway) 50 nameDelegCert))
    prop
      "GOVCERT"
      ( withMaxSuccess
          50
          (minitraceProp (GOVCERT Conway) 50 nameGovCert)
      )
    prop
      "CERT"
      (withMaxSuccess 50 (minitraceProp (CERT Conway) 50 nameTxCert))
    prop
      "CERTS"
      ( withMaxSuccess
          50
          (minitraceProp (CERTS Conway) 50 nameCerts)
      )
    prop
      "RATIFY"
      (withMaxSuccess 50 (minitraceProp (RATIFY Conway) 50 nameRatify))
    -- prop "ENACT" (withMaxSuccess 50 (minitraceProp (ENACT Conway) (Proxy @ConwayFn) 50 nameEnact))
    -- These properties do not have working 'signalSpec' Specifications yet.
    xprop
      "GOV"
      (withMaxSuccess 50 (minitraceProp (GOV Conway) 50 nameGovSignal))
    xprop
      "UTXO"
      (withMaxSuccess 50 (minitraceProp (UTXO Conway) 50 nameAlonzoTx))
    xprop
      "EPOCH"
      (withMaxSuccess 50 (minitraceProp (EPOCH Conway) 50 nameEpoch))
    xprop
      "NEWEPOCH"
      (withMaxSuccess 50 (minitraceProp (NEWEPOCH Conway) 50 nameEpoch))
