{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Trace.Pipeline where

import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core (Era, EraRule, Tx)
import Cardano.Ledger.Shelley.LedgerState (LedgerState)
import Cardano.Ledger.Shelley.Rules
import Control.Monad.Reader (Reader, runReader)
import Control.State.Transition.Extended (
  BaseM,
  RuleType (Transition),
  STS (..),
  TRC (..),
  applySTS,
 )
import Data.Default.Class (Default (def))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import GHC.TypeLits (Symbol)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (TxF (..))
import Test.Cardano.Ledger.Constrained.Env (Env, Name, emptyEnv, findVar)
import Test.Cardano.Ledger.Constrained.Monad (errorTyped, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.Repl (goRepl)
import Test.Cardano.Ledger.Constrained.Preds.Tx (adjustColInput, adjustFeeInput, txBodyPreds)
import Test.Cardano.Ledger.Constrained.Rewrite (
  DependGraph (..),
  initialOrder,
  mkDependGraph,
  notBefore,
  rewriteGen,
  standardOrderInfo,
 )
import Test.Cardano.Ledger.Constrained.Solver (solveOneVar)
import Test.Cardano.Ledger.Constrained.Stage (Pipeline, Stage (..), ledgerPipeline, solvePipeline)
import Test.Cardano.Ledger.Constrained.Trace.TraceMonad
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Proof hiding (WitRule (..), lift)
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import Test.QuickCheck

-- =============================================

data Rule (i :: Symbol) where
  DELEG :: Rule "DELEG"
  DELEGS :: Rule "DELEGS"
  DELPL :: Rule "DELPL"
  EPOCH :: Rule "EPOCH"
  LEDGER :: Rule "LEDGER"
  LEDGERS :: Rule "LEDGERS"
  MIR :: Rule "MIR"
  NEWEPOCH :: Rule "NEWSPOCH"
  NEWPP :: Rule "NEWPP"
  POOL :: Rule "POOL"
  POOLREAP :: Rule "POOLREAP"
  PPUP :: Rule "PPUP"
  RUPD :: Rule "RUPD"
  SNAP :: Rule "SNAP"
  TICK :: Rule "TICK"
  TICKF :: Rule "TICKF"
  UPEC :: Rule "UPEC"
  UTXO :: Rule "UTXO"
  UTXOW :: Rule "UTXOW"
  GOVCERT :: Rule "GOVCERT"
  GOV :: Rule "GOV"

trc ::
  Proof era ->
  Rule tag ->
  Environment (EraRule tag era) ->
  State (EraRule tag era) ->
  Signal (EraRule tag era) ->
  TRC (EraRule tag era)
trc _proof _rule env state sig = TRC (env, state, sig)

sts ::
  forall era tag.
  ( BaseM (EraRule tag era) ~ Reader Globals
  , STS (EraRule tag era)
  ) =>
  Proof era ->
  Rule tag ->
  Environment (EraRule tag era) ->
  State (EraRule tag era) ->
  Signal (EraRule tag era) ->
  Either [PredicateFailure (EraRule tag era)] (State (EraRule tag era))
sts proof rule env state sig =
  runReader
    ( applySTS @(EraRule tag era) @(Reader _) @'Transition
        (trc proof rule env state sig)
    )
    testGlobals

stsWithContinuations ::
  forall era tag ans.
  ( BaseM (EraRule tag era) ~ Reader Globals
  , STS (EraRule tag era)
  ) =>
  Proof era ->
  Rule tag ->
  ([PredicateFailure (EraRule tag era)] -> ans) ->
  (State (EraRule tag era) -> ans) ->
  Environment (EraRule tag era) ->
  State (EraRule tag era) ->
  Signal (EraRule tag era) ->
  ans
stsWithContinuations proof rule fails succeeds env state sig =
  case sts proof rule env state sig of
    Left xs -> fails xs
    Right x -> succeeds x

-- ====================================

-- | Translate a Pipe into a DependGraph, given the set
--   of variables that have aready been solved for.
pipeToGraph :: Era era => Stage era -> HashSet (Name era) -> TraceM era (DependGraph era)
pipeToGraph (Stage info ps) alreadyDefined = do
  simple <- liftCounter rewriteGen ps
  orderedNames <- liftTyped $ initialOrder info simple
  liftTyped $ mkDependGraph (length orderedNames) [] alreadyDefined orderedNames [] (Prelude.filter notBefore simple)

-- | Merge a Pipeline into an existing DependGraph, given the set of variables
--   that have aready been solved for, to get a larger DependGraph
mergePipeline :: Era era => Pipeline era -> HashSet (Name era) -> DependGraph era -> TraceM era (DependGraph era)
mergePipeline [] _ graph = pure graph
mergePipeline (pipe : more) defined (DependGraph xs) = do
  DependGraph ys <- pipeToGraph pipe defined
  let names = concatMap fst ys
  mergePipeline more (HashSet.union (HashSet.fromList names) defined) (DependGraph (xs ++ ys))

-- | Solve a Pipeline to get both an Env and a DependGraph
solvePipeline2 :: Reflect era => Pipeline era -> TraceM era (Env era, DependGraph era)
solvePipeline2 pipes = do
  gr@(DependGraph pairs) <- mergePipeline pipes HashSet.empty (DependGraph [])
  Subst subst <- liftGen (foldlM' solveOneVar emptySubst pairs)
  let isTempV k = notElem '.' k
  let sub = Subst (Map.filterWithKey (\k _ -> isTempV k) subst)
  env <- liftTyped (substToEnv sub emptyEnv)
  pure (env, gr)

main :: IO ()
main = do
  let proof = Babbage Standard
  ((env, DependGraph zs), _, _) <- generate (runTraceM 0 emptyEnv (solvePipeline2 (ledgerPipeline def proof)))
  let vs = varsOfTarget HashSet.empty dstateT
      ok = any (`HashSet.member` vs) . fst
  putStrLn (show (DependGraph (filter ok zs)))
  goRepl proof env ""

-- =================================================
-- Staging and STS rules

sts0 ::
  (Show ctx, Show state, Show sig, Testable prop) =>
  Gen ctx ->
  (ctx -> Gen state) ->
  (ctx -> state -> Gen sig) ->
  (ctx -> state -> sig -> state) ->
  (state -> state -> prop) ->
  Property
sts0 ctx state sig apply test =
  forAll
    ctx
    ( \c ->
        forAll
          (state c)
          ( \s ->
              withMaxSuccess
                100
                (forAll (sig c s) (\tx -> test s (apply c s tx)))
          )
    )

-- | When we run a STS rule, the context (env) is a subset of the state, so
--   it makes sense to generate state, and then extract the context.
sts1 ::
  (Show ctx, Show state, Show sig, Testable prop) =>
  Gen state ->
  (state -> Gen ctx) ->
  (state -> ctx -> Gen sig) ->
  ((ctx, state, sig) -> state) ->
  (state -> state -> prop) ->
  Property
sts1 state ctx sig apply test =
  forAll
    state
    ( \s ->
        forAll
          (ctx s)
          ( \c ->
              withMaxSuccess
                100
                (forAll (sig s c) (\tx -> test s (apply (c, s, tx))))
          )
    )

proofx :: Proof (BabbageEra StandardCrypto)
proofx = Babbage Standard

genLedgerState :: Gen (Env Babbage, Subst Babbage, LedgerState Babbage)
genLedgerState = do
  (env, sub, _graph) <- solvePipeline (ledgerPipeline def proofx)
  ledgerstate <- monadTyped $ runTarget env (ledgerStateT proofx)
  pure (env, sub, ledgerstate)

genSig :: Reflect era => Proof era -> (a, Subst era, b) -> p -> Gen (Tx era)
genSig proof (_, sub, _) _ledgerEnv = do
  let preds = fmap (substPred sub) (txBodyPreds def proof)
  (env0, _sub, _graph) <- solvePipeline [Stage standardOrderInfo preds]
  env1 <- monadTyped $ adjustFeeInput env0
  env2 <- errorTyped $ adjustColInput env1
  (TxF _ tx) <- monadTyped (findVar (unVar txterm) env2)
  pure tx
