{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Constrained.Stage where

import Cardano.Ledger.Core (Era (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Pulse (foldlM')
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Env (Env, Name, emptyEnv)
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (certStatePreds, pstatePreds, vstatePreds)
import Test.Cardano.Ledger.Constrained.Preds.LedgerState (ledgerStatePreds)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsPreds)
import Test.Cardano.Ledger.Constrained.Preds.Universes (UnivSize (..), universePreds)
import Test.Cardano.Ledger.Constrained.Rewrite (
  DependGraph (..),
  OrderInfo,
  initialOrder,
  mkDependGraph,
  notBefore,
  rewriteGen,
  standardOrderInfo,
 )
import Test.Cardano.Ledger.Constrained.Solver (solveOneVar)
import Test.Cardano.Ledger.Generic.Proof hiding (lift)
import Test.QuickCheck

-- ====================================

-- | Group together some Preds and OrderInfo about how to decide the
--   order in which to solve the variables appearing in the Preds
data Stage era = Stage OrderInfo [Pred era]

type Pipeline era = [Stage era]

-- | A pipeline for specifying the LederState
ledgerPipeline :: Reflect era => UnivSize -> Proof era -> Pipeline era
ledgerPipeline sizes proof =
  [ Stage standardOrderInfo (pParamsPreds proof)
  , Stage standardOrderInfo (universePreds sizes proof)
  ]
    ++ ( case whichPParams proof of
          PParamsConwayToConway -> [Stage standardOrderInfo (vstatePreds proof)]
          _ -> []
       )
    ++ [ Stage standardOrderInfo (pstatePreds proof)
       , Stage standardOrderInfo (certStatePreds proof)
       , Stage standardOrderInfo (ledgerStatePreds sizes proof)
       ]

-- | Translate a Stage into a DependGraph, given the set
--   of variables that have aready been solved for.
stageToGraph :: Era era => Int -> Stage era -> HashSet (Name era) -> Gen (Int, DependGraph era)
stageToGraph n0 (Stage info ps) alreadyDefined = do
  (n1, simple) <- rewriteGen (n0, ps)
  orderedNames <- monadTyped $ initialOrder info simple
  graph <-
    monadTyped $
      mkDependGraph
        (length orderedNames)
        []
        alreadyDefined
        orderedNames
        []
        (Prelude.filter notBefore simple)
  pure (n1, graph)

-- | Merge a Pipeline into an existing DependGraph, given the set of variables
--   that have aready been solved for, to get a larger DependGraph
mergePipeline :: Era era => Int -> Pipeline era -> HashSet (Name era) -> DependGraph era -> Gen (Int, DependGraph era)
mergePipeline n [] _ graph = pure (n, graph)
mergePipeline n0 (pipe : more) defined (DependGraph xs) = do
  (n1, DependGraph ys) <- stageToGraph n0 pipe defined
  let names = concat (map fst ys)
  mergePipeline n1 more (HashSet.union (HashSet.fromList names) defined) (DependGraph (xs ++ ys))

-- | Solve a Pipeline to get a Env, Subst, and a DependGraph
solvePipeline :: Reflect era => Pipeline era -> Gen (Env era, Subst era, DependGraph era)
solvePipeline pipes = do
  (_, gr@(DependGraph pairs)) <- mergePipeline 0 pipes HashSet.empty (DependGraph [])
  Subst subst <- foldlM' solveOneVar emptySubst pairs
  let isTempV k = not (elem '.' k)
  let sub = Subst (Map.filterWithKey (\k _ -> isTempV k) subst)
  env <- monadTyped (substToEnv sub emptyEnv)
  pure (env, sub, gr)
