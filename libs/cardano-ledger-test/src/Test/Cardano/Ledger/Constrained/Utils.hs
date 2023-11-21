{-# LANGUAGE BangPatterns #-}

module Test.Cardano.Ledger.Constrained.Utils (
  testIO,
  checkForSoundness,
  explainBad,
) where

import Cardano.Ledger.Conway.Core (Era)
import qualified Control.Exception as Exc
import Control.Monad (void)
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Constrained.Ast (Pred, Subst (..), SubstElem (..), makeTest, substToEnv, varsOfPred)
import Test.Cardano.Ledger.Constrained.Env (Env, Name (..), V (..), emptyEnv)
import Test.Cardano.Ledger.Constrained.Monad (Typed, monadTyped)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)

testIO :: String -> IO a -> TestTree
testIO msg x = testCase msg (Exc.catch (void x) handler)
  where
    -- handler :: Exc.ErrorCall -> IO ()
    handler (Exc.SomeException zs) = assertFailure (unlines [msg, show zs])

checkForSoundness :: Era era => [Pred era] -> Subst era -> Typed (Env era, Maybe String)
checkForSoundness preds subst = do
  !env <- monadTyped $ substToEnv subst emptyEnv
  testTriples <- mapM (makeTest env) preds
  let bad = filter (\(_, b, _) -> not b) testTriples
  if null bad
    then pure (env, Nothing)
    else pure (env, Just ("Some conditions fail\n" ++ explainBad bad subst))

explainBad :: Era era => [(String, Bool, Pred era)] -> Subst era -> String
explainBad cs (Subst subst) = unlines (map getString cs) ++ "\n" ++ show restricted
  where
    names = List.foldl' varsOfPred HashSet.empty (map getPred cs)
    restricted = Map.filterWithKey ok subst
    ok key (SubstElem rep _term access) = HashSet.member (Name (V key rep access)) names
    getString (s, _, _) = s
    getPred (_, _, pr) = pr
