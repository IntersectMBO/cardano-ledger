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
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Constrained.Examples where

-- import Debug.Trace(trace)
-- import Data.Set (Set)
import qualified Data.Set as Set

-- import Numeric.Natural (Natural)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.DPState (FutureGenDeleg (..))
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair)
import Cardano.Ledger.Pretty
import Cardano.Ledger.Shelley.Rewards (Reward (..))
import Control.Exception (ErrorCall (..))
import Control.Monad (when)
import qualified Data.List as List
import Data.Map (Map)
import Debug.Trace (trace)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (Adds (..), SumCond (..), Sums (genT))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL)
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyC (..), pcFutureGenDeleg, pcGenDelegPair)
import Test.Cardano.Ledger.Generic.Proof (C_Crypto, Reflect (..))
import Test.Hspec.Expectations (shouldThrow)
import Test.QuickCheck hiding (Fixed, total)

-- ===========================================

runCompile :: [Pred era] -> IO ()
runCompile cs = case runTyped (compile standardOrderInfo cs) of
  Right x -> print x
  Left xs -> putStrLn (unlines xs)

data Assembler era where
  Assemble :: PrettyC t era => Target era t -> Assembler era
  Skip :: Assembler era

runTarget :: Env era -> Target era t -> Typed t
runTarget _ (Constr _ c) = pure c
runTarget env (Simple e) = runTerm env e
runTarget env (f :$ x) = do
  g <- runTarget env f
  y <- runTarget env x
  pure (g y)

stoi :: OrderInfo
stoi = standardOrderInfo

failn :: Proof era -> String -> Bool -> OrderInfo -> [Pred era] -> Assembler era -> IO ()
failn proof message loud order cs target =
  shouldThrow
    (testn proof message loud order cs target)
    ( \(ErrorCall msg) ->
        trace
          ( if loud
              then ("Fails as expected\n" ++ msg ++ "\nOK")
              else ("Fails as expected OK")
          )
          True
    )

-- | A tracing test harness that displays all the steps in solving a [Pred era]
testn :: Proof era -> String -> Bool -> OrderInfo -> [Pred era] -> Assembler era -> IO ()
testn proof testname loud order cs target = do
  putStr ("*** " ++ testname ++ ":   ")
  when loud $ putStrLn "======================================================="
  let cs2 = (remDom cs)
  let cs3 = removeEqual cs2 []
  let cs4 = removeSameVar cs3 []
  let graph@(DependGraph _) = ioTyped $ compile order cs
  when loud $
    putStrLn
      ( unlines
          [ "Constraints"
          , show cs
          , "Introduce new variables"
          , show cs2
          , "Substitute for Equality"
          , show cs3
          , "Remove syntactic tautologies"
          , show cs4
          , "Pick a variable ordering\n"
          , show graph
          , "Solve for each variable in the order computed. Note by the time we get"
          , "to each variable, it is the only variable left in the constraint."
          ]
      )
  result <- generate (genDependGraph loud proof graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  when loud $ putStrLn "\nSubstitution produced after solving\n"
  when loud $ putStrLn (unlines (map show subst))
  let env = substToEnv subst emptyEnv
  let ss = ioTyped (mapM (makeTest env) cs)
      bad = filter (\(_, b, _) -> not b) ss
  if not (null bad)
    then error ("Some conditions fail\n" ++ explainBad bad subst)
    else putStrLn "OK"
  case target of
    Skip -> pure ()
    Assemble t -> do
      let tval = ioTyped (runTarget env t)
      when loud $ putStrLn "\nAssemble the pieces\n"
      when loud $ putStrLn (show (prettyC proof tval))

explainBad :: [(String, Bool, Pred era)] -> [SubItem era] -> String
explainBad cs subst = unlines (map getString cs) ++ "\n" ++ unlines (map show restricted)
  where
    names = List.foldl' varsOfPred Set.empty (map getPred cs)
    restricted = filter ok subst
    ok (SubItem v _) = Set.member (Name v) names
    getString (s, _, _) = s
    getPred (_, _, pr) = pr

-- ======================================================
-- Now some tests
-- ======================================================

-- | Used to test cyclic dependencies
cyclicPred :: [Pred era]
cyclicPred = [a :<=: b, b :<=: c, Random d, c :<=: a]
  where
    a = Var (V "a" (SetR IntR) No)
    b = Var (V "b" (SetR IntR) No)
    c = Var (V "c" (SetR IntR) No)
    d = Var (V "d" IntR No)

test1 :: IO ()
test1 = do
  putStrLn "\n======================================================="
  putStrLn "Test 1. Detect cycles"
  runCompile cyclicPred
  putStrLn "OK"

-- ===========================================
-- Test that the genT method of Sums works.

test2 :: Bool -> IO ()
test2 loud = do
  putStrLn "*** Test 2. genT of Sums class, generates list with sum 30"
  when loud $ putStrLn "======================================================="
  zs <- generate (genT @[Reward C_Crypto] ["test2"] (Coin 30) :: Gen [Reward C_Crypto])
  let getcoin (Reward _ _ c) = c
      total = List.foldl' (\c r -> add c (getcoin r)) (Coin 0) zs
  if total /= (Coin 30)
    then error ("Does not add to 30\n" ++ (unlines (map show zs)))
    else putStrLn "OK"

-- ===================================
test3 :: IO ()
test3 = testn (Mary Mock) "Test 3. PState example" False stoi cs (Assemble pstateT)
  where
    cs =
      [ Sized (ExactSize 10) poolsUniv
      , Sized (Range 1 7) (Dom mockPoolDistr) -- At least 1, but smaller than the poolsUniv
      , Dom mockPoolDistr :=: Dom regPools -- or mockPoolDistr can't sum to (1%1) if its empty
      , Dom mockPoolDistr :=: Dom poolDeposits
      , Dom mockPoolDistr :<=: poolsUniv
      , Dom futureRegPools :<=: poolsUniv
      , Dom retiring :<=: Dom regPools
      , SumsTo EQL (Fixed (Lit RationalR 1)) [SumMap mockPoolDistr]
      ]

-- ==============================
test4 :: IO ()
test4 = failn (Mary Mock) "Test 4. Inconsistent Size" False stoi cs Skip
  where
    cs =
      [ Sized (ExactSize 5) rewards
      , SumsTo GTH (Fixed (Lit CoinR (Coin 5))) [SumMap rewards]
      ]

test5 :: IO ()
test5 = failn (Mary Mock) "Test 5. Bad Sum, impossible partition." False stoi cs Skip
  where
    cs =
      [ Sized (ExactSize 5) rewards
      , SumsTo EQL (Fixed (Lit CoinR (Coin 4))) [SumMap rewards]
      ]

-- ===========================================================
-- Example list of Constraints for test4 and test5

constraints :: Proof era -> [Pred era]
constraints proof =
  [ Sized (ExactSize 10) credsUniv
  , Sized (ExactSize 10) poolsUniv
  , Sized (AtLeast 1) mockPoolDistr -- This is summed so it can't be empty.
  , Sized (AtLeast 1) regPools -- This has the same domain, so it also can't be empty
  , Dom rewards :<=: credsUniv
  , Rng delegations :<=: poolsUniv
  , Dom poolDeposits :<=: poolsUniv
  , Dom rewards :=: Dom stakeDeposits
  , Dom stakeDeposits :<=: credsUniv
  , Dom delegations :<=: credsUniv
  , Dom delegations :<=: Dom rewards
  , Dom regPools :=: Dom mockPoolDistr
  , Dom regPools :=: Dom poolDeposits
  , Dom regPools :<=: poolsUniv
  , Dom retiring :<=: Dom regPools
  , SumsTo EQL deposits [SumMap stakeDeposits, SumMap poolDeposits]
  , SumsTo EQL (Fixed (Lit RationalR 1)) [SumMap mockPoolDistr]
  , SumsTo
      EQL
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
  , Random $ pparams proof
  ]

-- | Test that we can find a viable variable ordering
test6 :: Bool -> IO ()
test6 loud = do
  putStrLn "*** Test 6. find a viable order of variables"
  when loud $ putStrLn "======================================================="
  case runTyped (compile standardOrderInfo $ constraints (Shelley Mock)) of
    Right x ->
      if loud
        then print x
        else putStrLn "OK"
    Left xs -> error (unlines xs)

-- | Test that we can compute a solution
test7 :: Bool -> IO ()
test7 loud = do
  putStrLn "Test 7. compute a solution"
  when loud $ putStrLn "======================================================="
  let proof = Shelley Mock
  when loud $ putStrLn (show $ constraints proof)
  let graph = ioTyped $ compile standardOrderInfo $ constraints proof
  when loud $ putStrLn (show graph)
  result <- generate (genDependGraph loud proof graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  when loud $ putStrLn (unlines (map show subst))
  let env = substToEnv subst emptyEnv
  let ss = ioTyped (mapM (makeTest env) $ constraints proof)
  let bad = filter (\(_, b, _) -> not b) ss
  if not (null bad)
    then error ("Some conditions fail\n" ++ unlines (map (\(s, _, _) -> s) bad))
    else putStrLn "OK"

-- ================================================

pstateConstraints :: [Pred era]
pstateConstraints =
  [ Sized (ExactSize 12) poolsUniv
  , (Dom regPools) :<=: poolsUniv
  , (Dom retiring) :<=: poolsUniv
  , (Dom futureRegPools) :<=: poolsUniv
  , (Dom poolDeposits) :<=: poolsUniv
  , (Dom regPools) :=: (Dom poolDeposits)
  , (Dom retiring) :<=: (Dom regPools)
  , Disjoint (Dom retiring) (Dom futureRegPools)
  , (Dom futureRegPools) :<=: (Dom regPools)
  ]

test8 :: IO ()
test8 = testn (Alonzo Standard) "Test 8. Pstate constraints" False stoi pstateConstraints (Assemble pstateT)

-- ==============================================================
-- Test the summation predicates

sumPreds :: Reflect era => Proof era -> [Pred era]
sumPreds proof =
  [ Random totalAda
  , fees :=: Fixed (Lit CoinR (Coin 400))
  , deposits :=: Fixed (Lit CoinR (Coin 200))
  , SumsTo LTE totalAda [One fees, One deposits, One utxoAmt]
  , SumsTo EQL (Fixed (Lit RationalR 1)) [Project RationalR poolDistr]
  , SumsTo GTH utxoAmt [Project CoinR (utxo proof)]
  , Sized (AtLeast 1) (utxo proof) -- Any map that is summed, to a nonzero amount, can't be empty.
  , Sized (AtLeast 1) poolDistr
  ]
  where
    utxoAmt = Var (V "utxoAmt" CoinR No)

test9 :: IO ()
test9 = testn (Alonzo Standard) "Test 9. Test of summing" False stoi (sumPreds (Alonzo Standard)) Skip

test10 :: IO ()
test10 =
  testn
    proof
    "Test 10. Test conditions EQL LTH LTE GTH GTE"
    False
    stoi
    [ Random x
    , Random y
    , SumsTo LTH tsumLTH [One x, One y]
    , SumsTo LTE tsumLTE [One x, One y]
    , SumsTo GTH tsumGTH [One x, One y]
    , SumsTo GTE tsumGTE [One x, One y]
    , SumsTo EQL tsumEQL [One x, One y]
    ]
    Skip
  where
    x = Var (V "x" rep No)
    y = Var (V "y" rep No)
    tsumLTH = Var (V "tsumLTH" rep No)
    tsumLTE = Var (V "tsumLTE" rep No)
    tsumGTH = Var (V "tsumGTH" rep No)
    tsumGTE = Var (V "tsumGTE" rep No)
    tsumEQL = Var (V "tsumEQL" rep No)
    rep = CoinR
    proof = Mary Mock

test11 :: IO ()
test11 =
  testn
    proof
    "Test 11. Instanaeous Sum Tests"
    False
    (stoi {sumBeforeParts = False})
    [ Sized (AtLeast 1) treasury
    , Random instanTreasury
    , Sized (AtLeast 1) instanReserves
    , Negate (deltaReserves) :=: deltaTreasury
    , SumsTo EQL instanReservesSum [SumMap instanReserves]
    , SumsTo LTH (Delta instanReservesSum) [One (Delta reserves), One deltaReserves]
    , SumsTo EQL instanTreasurySum [SumMap instanTreasury]
    , SumsTo LTH (Delta instanTreasurySum) [One (Delta treasury), One deltaTreasury]
    ]
    Skip
  where
    instanReservesSum = Var (V "instanReservesSum" CoinR No)
    instanTreasurySum = Var (V "instanTreasurySum" CoinR No)
    proof = Alonzo Standard

{- Still get this failure very occaisionaly
when    (stoi{sumBeforeParts = True})
(Delta ₳ 1) < ∑ (Delta reserves) + ▵₳ 3
Given (Add s), Solving for reserves::c. Predicates
  (Delta ₳ 1) < ∑ (Delta reserves) + ▵₳ 3
DeltaCoin is negative: DeltaCoin (-2). Can't convert to Coin
-}

-- =====================================================

test12 :: IO ()
test12 =
  testn
    p
    "Test 12. CanFollow tests"
    False
    stoi
    [ Random (pparams p)
    , Random (prevProtVer p)
    , (protVer p) `CanFollow` (prevProtVer p)
    , Component (pparams p) [field pp (protVer p)]
    , Component (prevpparams p) [field pp (prevProtVer p)]
    ]
    Skip
  where
    pp = PParamsR p
    p = Shelley Standard

-- ==============================================================
-- Test the Component Predicate

componentPreds :: Proof era -> [Pred era]
componentPreds proof =
  [ Random (minFeeA proof)
  , Random size
  , Sized size llx -- Note since size denotes a range, llx and mm could have different sizes
  , Sized size mm
  , SumsTo
      EQL
      (Fixed $ Lit CoinR (Coin 100))
      [ One (minFeeA proof)
      , One (minFeeB proof)
      ]
  , Component (pparams proof) [field pp (minFeeA proof), field pp (minFeeB proof)]
  ]
  where
    pp = PParamsR proof
    llx = Var (V "llx" (SetR IntR) No)
    size = Var (V "size" SizeR No)
    mm = Var (V "mm" (MapR IntR IntR) No)

test13 :: IO ()
test13 =
  testn
    proof
    "Test 13. Component tests"
    False
    stoi
    (componentPreds proof)
    Skip
  where
    proof = Shelley Standard

-- ==============================================

test14 :: IO ()
test14 =
  failn
    (Allegra Mock)
    "Test 14. Catch unsolveable use of Sized"
    False
    stoi
    [ Sized (ExactSize 6) credsUniv
    , Sized (ExactSize 10) rewards
    , Dom rewards :=: credsUniv
    ]
    Skip

-- ==============================================

test15 :: IO ()
test15 =
  testn
    proof
    "Test 15. Summation on Natural"
    False
    stoi
    [ Sized (AtLeast 1) (maxTxSize proof)
    , Sized (AtLeast 1) (maxBHSize proof)
    , -- if both maxTxSize and maxBHSize are Random it may be impossible to solve
      -- because  both might be 0, and maxBBSize <= 0, is inconsistent.
      SumsTo LTE (maxBBSize proof) [One (maxBHSize proof), One (maxTxSize proof)]
    , Component
        (pparams proof)
        [field pp (maxTxSize proof), field pp (maxBHSize proof), field pp (maxBBSize proof)]
    ]
    Skip
  where
    proof = Alonzo Standard
    pp = PParamsR proof

-- ============================================================

preds16 :: Reflect era => Proof era -> [Pred era]
preds16 _proof =
  [ Sized (ExactSize 6) poolsUniv
  , Sized (Range 1 3) (Dom poolDistr) -- At least 1 but smaller than the poolsUniv
  , Dom poolDistr :<=: poolsUniv
  , SumsTo EQL (Fixed (Lit RationalR 1)) [Project RationalR poolDistr]
  , Sized (Range 1 4) (Dom foox) -- At least 1 but smaller than the poolsUniv
  , Dom foox :<=: poolsUniv
  , SumsTo EQL (Fixed (Lit RationalR 1)) [SumMap foox]
  ]
  where
    foox = Var (V "foo" (MapR PoolHashR RationalR) No)

test16 :: IO ()
test16 =
  testn
    proof
    "Test 11C. Test NonEmpty subset"
    False
    stoi
    (preds16 proof)
    Skip
  where
    proof = Alonzo Standard

-- ==============================================

univPreds :: Proof era -> [Pred era]
univPreds p =
  [ Sized (ExactSize 12) credsUniv
  , Sized (ExactSize 12) poolsUniv
  , Sized (ExactSize 8) genesisUniv
  , Sized (ExactSize 12) txinUniv
  , Sized (Range 1 6) (Dom poolDistr) -- Greater than 1, smaller than size of poolsUniv
  , Dom poolDistr :<=: poolsUniv
  , Dom regPools :<=: poolsUniv
  , Dom retiring :<=: poolsUniv
  , Dom futureRegPools :<=: poolsUniv
  , Dom poolDeposits :<=: poolsUniv
  , Dom prevBlocksMade :<=: poolsUniv
  , Dom currBlocksMade :<=: poolsUniv
  , Dom markPools :<=: poolsUniv
  , Dom markPoolDistr :<=: poolsUniv
  , Dom setPools :<=: poolsUniv
  , Dom goPools :<=: poolsUniv
  , Dom poolDistr :<=: poolsUniv
  , Dom stakeDeposits :<=: credsUniv
  , Dom delegations :<=: credsUniv
  , Dom rewards :<=: credsUniv
  , Dom markStake :<=: credsUniv
  , Dom markDelegs :<=: credsUniv
  , Dom setStake :<=: credsUniv
  , Dom setDelegs :<=: credsUniv
  , Dom goStake :<=: credsUniv
  , Dom goDelegs :<=: credsUniv
  , Dom instanReserves :<=: credsUniv
  , Dom instanTreasury :<=: credsUniv
  , Dom (proposalsT p) :<=: genesisUniv
  , Dom (futureProposalsT p) :<=: genesisUniv
  , Dom genDelegs :<=: genesisUniv
  , Dom (utxo p) :<=: txinUniv
  ]

pstatePreds :: Proof era -> [Pred era]
pstatePreds _p =
  [ Dom poolDistr :=: Dom regPools
  , Dom poolDistr :=: Dom poolDeposits
  , Dom retiring :<=: Dom poolDistr
  , Dom futureRegPools :<=: Dom poolDistr
  , Disjoint (Dom retiring) (Dom futureRegPools)
  ]

dstatePreds :: Proof era -> [Pred era]
dstatePreds _p =
  [ Dom rewards :=: Dom stakeDeposits
  , Dom delegations :<=: Dom rewards
  , Dom rewards :=: Rng ptrs
  , -- This implies (Fixed (ExactSize 3) instanReserves)
    -- But it also implies that the new introduced variable instanReservesDom also has size 3
    -- ,  Sized (ExactSize 3) (Dom instanReserves)
    -- , Sized (ExactSize 2) (Dom instanTreasury)

    Sized (AtLeast 1) treasury
  , Random instanTreasury
  , Sized (AtLeast 1) instanReserves
  , Negate (deltaReserves) :=: deltaTreasury
  , SumsTo EQL instanReservesSum [SumMap instanReserves]
  , SumsTo LTH (Delta instanReservesSum) [One (Delta reserves), One deltaReserves]
  , SumsTo EQL instanTreasurySum [SumMap instanTreasury]
  , SumsTo LTH (Delta instanTreasurySum) [One (Delta treasury), One deltaTreasury]
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]
  where
    instanReservesSum = Var (V "instanReservesSum" CoinR No)
    instanTreasurySum = Var (V "instanTreasurySum" CoinR No)

accountstatePreds :: Proof era -> [Pred era]
accountstatePreds _p = [] -- Constraints on reserves and treasury appear in dstatePreds

utxostatePreds :: Reflect era => Proof era -> [Pred era]
utxostatePreds proof =
  [ SumsTo EQL utxoCoin [Project CoinR (utxo proof)]
  , SumsTo EQL deposits [SumMap stakeDeposits, SumMap poolDeposits]
  , SumsTo EQL totalAda [One utxoCoin, One treasury, One reserves, One fees, One deposits, SumMap rewards]
  , Random fees
  , Random (proposalsT proof)
  , Random (futureProposalsT proof)
  ]

epochstatePreds :: Reflect era => Proof era -> [Pred era]
epochstatePreds proof =
  [ Random markStake
  , Random markDelegs
  , Random markPools
  , Random setStake
  , Random setDelegs
  , Random setPools
  , Random goStake
  , Random goDelegs
  , Random goPools
  , Random snapShotFee
  , Random (prevpparams proof)
  , Random (pparams proof)
  , Sized (AtLeast 1) (maxBHSize proof)
  , Sized (AtLeast 1) (maxTxSize proof)
  , -- , Random (maxBBSize proof) -- This will cause underflow on Natural
    SumsTo EQL (Fixed (Lit RationalR 1)) [Project RationalR markPoolDistr]
  , SumsTo LTE (maxBBSize proof) [One (maxBHSize proof), One (maxTxSize proof)]
  , Component
      (pparams proof)
      [field pp (maxTxSize proof), field pp (maxBHSize proof), field pp (maxBBSize proof)]
  ]
  where
    pp = PParamsR proof

newepochstatePreds :: Reflect era => Proof era -> [Pred era]
newepochstatePreds _proof =
  [ Random epochNo
  , Sized (ExactSize 8) (Dom prevBlocksMade) -- Both prevBlocksMade and prevBlocksMadeDom will have size 8
  , Sized (ExactSize 8) (Dom currBlocksMade)
  , SumsTo EQL (Fixed (Lit RationalR 1)) [Project RationalR poolDistr]
  ]

newepochConstraints :: Reflect era => Proof era -> [Pred era]
newepochConstraints pr =
  univPreds pr
    ++ pstatePreds pr
    ++ dstatePreds pr
    ++ utxostatePreds pr
    ++ accountstatePreds pr
    ++ epochstatePreds pr
    ++ newepochstatePreds pr

test17 :: IO ()
test17 =
  testn
    proof
    "Test 17. Full NewEpochState"
    False
    (stoi {sumBeforeParts = False})
    (newepochConstraints proof)
    (Assemble (newEpochStateT proof))
  where
    proof = Alonzo Standard

{-
 Exception: Solving for variable reserves
(Delta ₳ 16) < ∑ (Delta reserves) + ▵₳ 27

Given (Add s), Solving for reserves::c. Predicates
  (Delta ₳ 16) < ∑ (Delta reserves) + ▵₳ 27

DeltaCoin is negative: DeltaCoin (-11). Can't convert to Coin
-}

-- ==========================================================
-- Tests of the Term projection function ProjS

projPreds1 :: Proof era -> [Pred era]
projPreds1 _proof =
  [ Sized (ExactSize 4) futureGenDelegs
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]

projPreds2 :: Proof era -> [Pred era]
projPreds2 _proof =
  [ Dom genDelegs :<=: genesisUniv
  , Sized (ExactSize 12) futGDUniv
  , --  , (Dom futureGenDelegs) :<=: futGDUniv
    Sized (ExactSize 6) genesisUniv
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]
  where
    futGDUniv = (Var (V "futGDUniv" (SetR FutureGenDelegR) No))

test18 :: IO ()
test18 = do
  testn proof "Test 18a. Projection test" False stoi (projPreds1 proof) Skip
  testn proof "Test 18b. Projection test" False stoi (projPreds2 proof) (Assemble (Constr "Pair" (,) $> futureGenDelegs $> genDelegs))
  where
    proof = Alonzo Standard

-- ===============================================
-- test of projOnDom

help19 ::
  forall era.
  Era era =>
  Proof era ->
  Gen (Map (FutureGenDeleg (EraCrypto era)) (GenDelegPair (EraCrypto era)))
help19 _proof = do
  setA <- genRep @era (SetR GenHashR)
  ans <- projOnDom @era setA (fGenDelegGenKeyHashL) FutureGenDelegR GenDelegPairR
  pure ans

test19 :: IO ()
test19 = do
  putStrLn "Test 19. test of projOnDom function"
  putStrLn "======================================================="
  ans <- generate (help19 (Babbage Standard))
  putStrLn (show (ppMap pcFutureGenDeleg pcGenDelegPair ans))

-- ===================================================
-- run all the tests

testAll :: IO ()
testAll = do
  test1
  (test2 False)
  test19
  test4
  test5
  (test6 False)
  (test7 False)
  test14
  -- (run 100 test3)
  (run 100 test8)
  (run 100 test9)
  (run 100 test10)
  (run 100 test11)
  (run 100 test12)
  (run 100 test13)
  (run 100 test15)
  (run 100 test16)
  (run 10 test17) -- This fails with can't convert (DeltaCoin -11) for variable reserves. See comment below
  (run 100 test18)

{-
 Exception: Solving for variable reserves
(Delta ₳ 16) < ∑ (Delta reserves) + ▵₳ 27

Given (Add s), Solving for reserves::c. Predicates
  (Delta ₳ 16) < ∑ (Delta reserves) + ▵₳ 27

DeltaCoin is negative: DeltaCoin (-11). Can't convert to Coin
-}

run :: Monad m => Int -> m a -> m ()
run n x = sequence_ (replicate n x)
