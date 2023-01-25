{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Constrained.Examples where

import Cardano.Ledger.DPState (PState (..))
import Cardano.Ledger.Pretty
import Test.Cardano.Ledger.Constrained.Ast

-- import Test.Cardano.Ledger.Constrained.Classes  -- (Adds(..),Sums(..))

import Cardano.Ledger.Coin (Coin (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Spec
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcCoin, pcKeyHash, pcPoolParams)
import Test.Cardano.Ledger.Generic.Proof (
  Evidence (..),
  Proof (..),
  Reflect (..),
 )
import Test.QuickCheck hiding (Fixed, total)

-- ===========================================

runCompile :: [Pred era] -> IO ()
runCompile cs = case runTyped (compile standardOrderInfo cs) of
  Right x -> print x
  Left xs -> putStrLn (unlines xs)

-- | Used to test cyclic dependencies
cyclicPred :: [Pred era]
cyclicPred = [a :<=: b, b :<=: c, Random d, c :<=: a]
  where
    a = Var (V "a" (SetR IntR) No)
    b = Var (V "b" (SetR IntR) No)
    c = Var (V "c" (SetR IntR) No)
    d = Var (V "d" IntR No)

test1 :: IO ()
test1 = runCompile cyclicPred

runTarget :: Env era -> Target era t -> Typed t
runTarget _ (Constr _ c) = pure c
runTarget env (Simple e) = runTerm env e
runTarget env (f :$ x) = do
  g <- runTarget env f
  y <- runTarget env x
  pure (g y)

test3 :: IO ()
test3 = do
  let cs =
        [ Sized (Fixed (Lit Word64R 10)) poolsUniv
        , Dom poolDistr :<=: Dom regPools
        , Dom regPools :=: Dom poolDistr
        , Dom regPools :=: Dom poolDeposits
        , Dom regPools :<=: poolsUniv
        , Dom retiring :<=: Dom regPools
        , SumsTo (Fixed (Lit RationalR 1)) [SumMap poolDistrVar]
        , Dom futureRegPools :<=: poolsUniv
        ]
  -- putStrLn (show cs)
  let cs2 = (removeDom cs)
  -- putStrLn (show cs2)
  let cs3 = removeEqual cs2 []
  -- putStrLn (show cs3)
  let cs4 = removeSameVar cs3 []
  -- putStrLn (show cs4)
  graph@(DependGraph _) <- ioTyped $ compile standardOrderInfo cs
  putStrLn
    ( unlines
        [ "Constraints"
        , show cs
        , "Introduce new variables"
        , show cs2
        , "Substitute for Equality"
        , show cs3
        , "Remove syntactic tautologys"
        , show cs4
        , "Pick a variable ordering\n"
        , show graph
        , "Solve for each variable in the order computed. Note by the time we get\nto each variable, it is the only variable left in the constraint."
        ]
    )
  result <- generate (genDependGraph (Shelley Mock) graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  putStrLn "\nSubstitution produced after solving\n"
  putStrLn (unlines (map show subst))
  let env = substToEnv subst emptyEnv
  putStrLn "Testing constraints against the values generated\n"
  ss <- ioTyped (mapM (makeTest env) cs)
  putStrLn (unlines ss)
  pstate <- ioTyped (runTarget env pstateT)
  putStrLn "\nAssemble the pieces into a PState\n"
  putStrLn (show (pcPState pstate))

solvePair :: (Name era, [Pred era]) -> Typed (Name era, String)
solvePair (Name v@(V _nm rep@(MapR _ _) _), cs) = explain ("Solving " ++ show cs) $ do sol <- solveMaps v cs; pure (Name v, showMapSpec rep sol)
solvePair (Name v@(V _nm (SetR repa) _), cs) = do sol <- solveSets v cs; pure (Name v, showSetSpec repa sol)
solvePair (Name v@(V _nm rep _), cs) | isSumType rep = do
  With v2 <- hasSummable rep v
  xs <- solveSums v2 cs
  pure (Name v, showSumSpec xs)
solvePair (Name v1@(V _nm _ _), [Random (Var v2)]) | Name v1 == Name v2 = pure $ (Name v1, "Random")
solvePair (Name (V _nm rep _), cs) = failT ["No solution at type " ++ show rep ++ " for condtions " ++ show cs]

test6 :: OrderInfo -> IO ()
test6 info = case runTyped (compile info constraints) of
  Right x -> print x
  Left xs -> putStrLn (unlines xs)

test7 :: IO ()
test7 = do
  putStrLn (show constraints)
  graph <- ioTyped $ compile standardOrderInfo constraints
  putStrLn (show graph)
  result <- generate (genDependGraph (Shelley Mock) graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  putStrLn (unlines (map show subst))
  let env = substToEnv subst emptyEnv
  ss <- ioTyped (mapM (makeTest env) constraints)
  putStrLn (unlines ss)

-- ===========================================================
-- Example list of Constraints

constraints :: [Pred era]
constraints =
  [ -- Sized (Fixed (Lit Word64R 1000)) totalAda,
    Sized (Fixed (Lit Word64R 10)) credsUniv
  , Sized (Fixed (Lit Word64R 10)) poolsUniv
  , Dom rewards :<=: credsUniv
  , Rng delegations :<=: poolsUniv
  , Dom poolDeposits :<=: poolsUniv
  , Dom rewards :=: Dom stakeDeposits
  , Dom stakeDeposits :<=: credsUniv
  , Dom delegations :<=: credsUniv
  , Dom delegations :<=: Dom rewards
  , Dom poolDistr :<=: Dom regPools
  , Dom regPools :=: Dom poolDistr
  , Dom regPools :=: Dom poolDeposits
  , Dom regPools :<=: poolsUniv
  , Dom retiring :<=: Dom regPools
  , SumsTo deposits [SumMap stakeDeposits, SumMap poolDeposits]
  , SumsTo (Fixed (Lit RationalR 1)) [SumMap poolDistrVar]
  , -- , SumsTo (Fixed (Lit RationalR 1)) [Project poolDistr]
    -- , SumsTo totalAda [One fees,One deposits,Project utxo]
    --    999 = 500 + deposits + 300
    --    999 = 500 + 199 +      Project utxo => 300
    SumsTo
      totalAda
      [ One utxoCoin
      , One treasury
      , One reserves
      , One fees
      , SumMap stakeDeposits
      , SumMap poolDeposits
      , SumMap rewards
      ]
  , Random treasury
  , Random reserves
  , Random fees
  , Random utxoCoin
  ]

pcPState :: PState era -> PDoc
pcPState (PState regP fregP ret dep) =
  ppRecord
    "PState"
    [ ("regPools", ppMap pcKeyHash pcPoolParams regP)
    , ("futureRegPools", ppMap pcKeyHash pcPoolParams fregP)
    , ("retiring", ppMap pcKeyHash ppEpochNo ret)
    , ("poolDeposits", ppMap pcKeyHash pcCoin dep)
    ]

-- ================================================

pstateConstraints :: [Pred era]
pstateConstraints =
  [ Sized (Fixed (Lit Word64R 12)) poolsUniv
  , (Dom regPools) :<=: poolsUniv
  , (Dom retiring) :<=: poolsUniv
  , (Dom futureRegPools) :<=: poolsUniv
  , (Dom poolDeposits) :<=: poolsUniv
  , (Dom regPools) :=: (Dom poolDeposits)
  , (Dom retiring) :<=: (Dom regPools)
  , Disjoint (Dom retiring) (Dom futureRegPools)
  , (Dom futureRegPools) :<=: (Dom regPools)
  ]

test4 :: IO ()
test4 = do
  let cs = pstateConstraints

  -- putStrLn (show cs)
  let cs2 = (removeDom cs)
  -- putStrLn (show cs2)
  let cs3 = removeEqual cs2 []
  -- putStrLn (show cs3)
  let cs4 = removeSameVar cs3 []
  -- putStrLn (show cs4)
  graph@(DependGraph _) <- ioTyped $ compile standardOrderInfo cs
  putStrLn
    ( unlines
        [ "Constraints"
        , show cs
        , "Introduce new variables"
        , show cs2
        , "Substitute for Equality"
        , show cs3
        , "Remove syntactic tautologys"
        , show cs4
        , "Pick a variable ordering\n"
        , show graph
        , "Solve for each variable in the order computed. Note by the time we get\nto each variable, it is the only variable left in the constraint."
        ]
    )
  result <- generate (genDependGraph (Shelley Mock) graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  putStrLn "\nSubstitution produced after solving\n"
  putStrLn (unlines (map show subst))
  let env = substToEnv subst emptyEnv
  putStrLn "Testing constraints against the values generated\n"
  ss <- ioTyped (mapM (makeTest env) cs)
  putStrLn (unlines ss)
  pstate <- ioTyped (runTarget env pstateT)
  putStrLn "\nAssemble the pieces into a PState\n"
  putStrLn (show (pcPState pstate))

-- ====================================================================
newepochConstraints :: [Pred era]
newepochConstraints =
  [ Sized (Fixed (Lit Word64R 12)) credsUniv
  , Sized (Fixed (Lit Word64R 12)) poolsUniv
  , -- PState
    (Dom regPools) :<=: poolsUniv
  , (Dom retiring) :<=: poolsUniv
  , (Dom futureRegPools) :<=: poolsUniv
  , (Dom poolDeposits) :<=: poolsUniv
  , (Dom regPools) :=: (Dom poolDeposits)
  , (Dom retiring) :<=: (Dom regPools)
  , Disjoint (Dom retiring) (Dom retiring)
  , (Dom futureRegPools) :<=: (Dom regPools)
  , -- DState
    Dom rewards :<=: credsUniv
  , Dom stakeDeposits :<=: credsUniv
  , Dom delegations :<=: credsUniv
  , Dom rewards :=: Dom stakeDeposits
  , Dom delegations :<=: Dom rewards
  , Dom rewards :=: Rng ptrs
  -- Set.map fGenDelegGenKeyHash (Dom futureGenDelegs) :=: Dom genDelegs
  ]

{- Newepochstate constraint
1 % 1 =∑= sum poolDistr
-}

{-

Proj fGenDelegGenKeyHash (Dom futureGenDelegs) :=: Dom genDelegs
Sum (Proj coinfromTxout utxo) = 35  Rng utxo might have duplicates

Sum (Proj individualPoolStake markPoolDistr) = 1

-}

-- ==============================================================

sumPreds :: Reflect era => Proof era -> [Pred era]
sumPreds proof =
  [ totalAda :=: Fixed (Lit CoinR (Coin 1000))
  , -- Sized (Fixed (Lit Word64R 10)) (utxo proof)
    fees :=: Fixed (Lit CoinR (Coin 400))
  , deposits :=: Fixed (Lit CoinR (Coin 200))
  , SumsTo totalAda [One fees, One deposits, Project (utxo proof)]
  , SumsTo (Fixed (Lit RationalR 1)) [Project poolDistr]
  ]

testn :: Proof era -> [Pred era] -> IO ()
testn proof cs = do
  let cs2 = (removeDom cs)
  let cs3 = removeEqual cs2 []
  let cs4 = removeSameVar cs3 []
  graph@(DependGraph _) <- ioTyped $ compile standardOrderInfo cs
  putStrLn
    ( unlines
        [ "Constraints"
        , show cs
        , "Introduce new variables"
        , show cs2
        , "Substitute for Equality"
        , show cs3
        , "Remove syntactic tautologys"
        , show cs4
        , "Pick a variable ordering\n"
        , show graph
        , "Solve for each variable in the order computed. Note by the time we get\nto each variable, it is the only variable left in the constraint."
        ]
    )
  result <- generate (genDependGraph proof graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  putStrLn "\nSubstitution produced after solving\n"
  putStrLn (unlines (map show subst))
  let env = substToEnv subst emptyEnv
  putStrLn "Testing constraints against the values generated\n"
  ss <- ioTyped (mapM (makeTest env) cs)
  putStrLn (unlines ss)

go :: IO ()
go = testn (Alonzo Standard) (sumPreds (Alonzo Standard))
