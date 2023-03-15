{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Constrained.Shrink (
  shrinkEnv,
) where

import Control.Monad
import Data.Maybe

import Cardano.Ledger.Core (Era (..))
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.TypeRep

-- | Shrink an environment subject to the constraints in the given dependency graph.
--   The strategy is
--    * pick a variable
--    * shrink its value to something that still satisfies the defining constraints for the variable
--    * adjust the values for variables depending on the picked variables to fix constraint
--      violations resulting from the changed value
--   Note that the DependGraph tells us the dependency order of the variables: a variable only
--   depends on the variables before it in the graph, so when shrinking the value of a variable we
--   only need to adjust the values of later variables.
shrinkEnv :: Era era => DependGraph era -> Env era -> [Env era]
shrinkEnv (DependGraph vs) env =
  [ env'
  | (before, (x, cs), after) <- splits vs
  , env' <- shrinkOneVar env (map fst before) x cs after
  ]
  where
    splits :: [a] -> [([a], a, [a])]
    splits [] = []
    splits (x : xs) = ([], x, xs) : [(x : ys, y, zs) | (ys, y, zs) <- splits xs]

shrinkOneVar :: Era era => Env era -> [Name era] -> Name era -> [Pred era] -> [(Name era, [Pred era])] -> [Env era]
shrinkOneVar originalEnv before x cs after =
  [ env'
  | val' <- shrinkVar x cs' val
  , let fixup env (y, ycs) = do
          p <- findName y originalEnv
          p' <- fixupVar y (map (substPred $ envToSubst env) ycs) p
          pure $ storeName y p' env
  , env' <- maybeToList $ foldM fixup (storeName x val' beforeEnv) after
  ]
  where
    beforeEnv = restrictEnv before originalEnv
    beforeSubst = envToSubst beforeEnv
    cs' = map (substPred beforeSubst) cs
    val = fromJust $ findName x originalEnv

shrinkVar :: Era era => Name era -> [Pred era] -> Payload era -> [Payload era]
shrinkVar v cs p = [p' | p' <- shrinkPayload p, validAssignment v p' cs]

shrinkPayload :: Era era => Payload era -> [Payload era]
shrinkPayload (Payload rep t acc) = [Payload rep t' acc | t' <- shrinkRep rep t]

-- | Compute something satisfying the constraints that's as "close" to the original value as
--   possible. TODO: more cleverness
fixupVar :: Era era => Name era -> [Pred era] -> Payload era -> Maybe (Payload era)
fixupVar v cs p = listToMaybe [p' | p' <- [p | validAssignment v p cs] ++ reverse (shrinkVar v cs p)]

-- | Assumes the variable is the only free variable in the constraints.
validAssignment :: Name era -> Payload era -> [Pred era] -> Bool
validAssignment v p cs = all (runPred_ $ storeName v p emptyEnv) cs

runPred_ :: Env era -> Pred era -> Bool
runPred_ env p = either (const False) id $ runTyped $ runPred env p
