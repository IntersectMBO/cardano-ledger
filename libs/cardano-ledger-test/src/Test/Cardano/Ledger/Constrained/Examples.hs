{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Constrained.Examples where

import Cardano.Ledger.Babbage (Babbage)
import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.CertState (FutureGenDeleg (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Keys (GenDelegPair)
import Control.DeepSeq (deepseq)
import Control.Exception (ErrorCall (..))
import Control.Monad (when)
import Data.Default.Class (Default (..))
import Data.Map (Map)
import Data.Ratio ((%))
import Debug.Trace (trace)
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL)
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Tx (genTxAndNewEpoch)
import Test.Cardano.Ledger.Constrained.Rewrite
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.Spec (TT)
import Test.Cardano.Ledger.Constrained.Tests (prop_shrinking, prop_soundness)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Utils (explainBad, testIO)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..))
import Test.Cardano.Ledger.Generic.Proof (ConwayEra, Reflect (..), StandardCrypto)
import Test.Cardano.Ledger.Generic.Trace (testPropMax)
import Test.Hspec (shouldThrow)
import Test.QuickCheck hiding (Fixed, total)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Type.Reflection (typeRep)

-- =======================================================

-- ===========================================

runCompile :: Era era => [Pred era] -> IO ()
runCompile cs = case runTyped (compile standardOrderInfo cs) of
  Right x -> print x
  Left xs -> putStrLn (unlines xs)

data Assembler era where
  Assemble :: PrettyA t => RootTarget era x t -> Assembler era
  Skip :: Assembler era

stoi :: OrderInfo
stoi = standardOrderInfo

genMaybeCounterExample ::
  Reflect era =>
  Proof era ->
  String ->
  Bool ->
  OrderInfo ->
  [Pred era] ->
  Assembler era ->
  Gen (Maybe String)
genMaybeCounterExample proof _testname loud order cs target = do
  let cs3 = removeEqual cs []
  let cs4 = removeSameVar cs3 []
  graph@(DependGraph _) <- monadTyped $ compile order cs
  let messages1 =
        if listEq cpeq cs4 cs
          then ["Constraints", show cs, "Rewriting is idempotent"]
          else
            [ "Constraints"
            , show cs
            , "Substitute for Equality"
            , show cs3
            , "Remove syntactic tautologies"
            , show cs4
            , "Pick a variable ordering\n"
            , show graph
            , "Solve for each variable in the order computed. Note by the time we get"
            , "to each variable, it is the only variable left in the constraint."
            ]
  result <-
    if loud
      then trace (unlines messages1) (genDependGraph loud proof graph)
      else genDependGraph loud proof graph
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  !env <- monadTyped $ substToEnv subst emptyEnv
  testTriples <- monadTyped (mapM (makeTest env) cs)
  let messages2 = "\nSubstitution produced after solving\n" ++ show subst
  !messages3 <- case target of
    Skip -> pure []
    Assemble t -> do
      !tval <- monadTyped (runTarget env t)
      pure ["\nAssemble the pieces\n", show (prettyA tval)]
  let bad = filter (\(_, b, _) -> not b) testTriples
      ans =
        if null bad
          then Nothing -- Indicates success, nothing bad happened
          else Just ("Some conditions fail\n" ++ explainBad bad subst)
  if loud
    then trace (unlines (messages2 : messages3)) (pure ans)
    else pure ans

-- | Test that 'cs' :: [Pred] has a solution
testn ::
  Reflect era =>
  Proof era ->
  String ->
  Bool ->
  OrderInfo ->
  [Pred era] ->
  Assembler era ->
  Gen Property
testn proof testname loud order cs target = do
  result <- genMaybeCounterExample proof testname loud order cs target
  case result of
    Nothing -> pure $ counterexample "" True
    Just xs -> pure $ counterexample xs False

-- | Test that 'cs' :: [Pred] does NOT have a solution. We expect a failure
failn ::
  Reflect era => Proof era -> String -> Bool -> OrderInfo -> [Pred era] -> Assembler era -> IO ()
failn proof message loud order cs target = do
  shouldThrow
    ( do
        result <- generate (genMaybeCounterExample proof message loud order cs target)
        case result of
          Nothing -> putStrLn (message ++ " should have failed but it did not.")
          Just counter ->
            if loud
              then putStrLn counter
              else pure ()
    )
    ( \(ErrorCall msg) ->
        trace
          ( if loud
              then ("Fails as expected\n" ++ msg ++ "\nOK")
              else ("Fails as expected OK")
          )
          True
    )

testSpec :: String -> Gen Property -> IO ()
testSpec name p = do
  putStrLn ("testing: '" ++ name ++ "'")
  quickCheck p

-- ======================================================
-- Now some tests
-- ======================================================

-- | Used to test cyclic dependencies
aCyclicPred :: Era era => [Pred era]
aCyclicPred = [a :⊆: b, b :⊆: c, Random c]
  where
    a = Var (V "a" (SetR IntR) No)
    b = Var (V "b" (SetR IntR) No)
    c = Var (V "c" (SetR IntR) No)

cyclicPred :: Era era => [Pred era]
cyclicPred = [a :⊆: b, b :⊆: c, c :⊆: d, d :⊆: a, Random d]
  where
    a = Var (V "a" (SetR IntR) No)
    b = Var (V "b" (SetR IntR) No)
    c = Var (V "c" (SetR IntR) No)
    d = Var (V "d" (SetR IntR) No)

test1 :: IO ()
test1 = do
  putStrLn "testing: Detect cycles"
  runCompile @Babbage aCyclicPred
  putStrLn "+++ OK, passed 1 test."

-- ===================================
test3 :: Gen Property
test3 = testn Mary "Test 3. PState example" False stoi cs (Assemble pstateT)
  where
    cs =
      [ Sized (ExactSize 10) poolHashUniv
      , Sized (Range 1 7) (Dom mockPoolDistr) -- At least 1, but smaller than the poolHashUniv
      , Dom mockPoolDistr :=: Dom regPools -- or mockPoolDistr can't sum to (1%1) if its empty
      , Dom mockPoolDistr :=: Dom poolDeposits
      , Dom mockPoolDistr :⊆: poolHashUniv
      , Dom futureRegPools :⊆: poolHashUniv
      , Dom retiring :⊆: Dom regPools
      , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [SumMap mockPoolDistr]
      ]

-- ==============================

test4 :: IO ()
test4 = failn Mary "Test 4. Inconsistent Size" False stoi cs Skip
  where
    cs =
      [ Sized (ExactSize 5) rewards
      , SumsTo (Right (Coin 1)) (Lit CoinR (Coin 5)) GTH [SumMap rewards]
      ]

test5 :: IO ()
test5 = failn Mary "Test 5. Bad Sum, impossible partition." False stoi cs Skip
  where
    cs =
      [ Sized (ExactSize 5) rewards
      , SumsTo (Right (Coin 1)) (Lit CoinR (Coin 4)) EQL [SumMap rewards]
      ]

-- ===========================================================
-- Example list of Constraints for test4 and test5

constraints :: EraGov era => Proof era -> [Pred era]
constraints proof =
  [ Sized (ExactSize 10) credsUniv
  , Sized (ExactSize 10) poolHashUniv
  , Sized (AtLeast 1) mockPoolDistr -- This is summed so it can't be empty.
  , Sized (AtLeast 1) regPools -- This has the same domain, so it can't be empty either
  , Dom rewards :⊆: credsUniv
  , Rng delegations :⊆: poolHashUniv
  , Dom poolDeposits :⊆: poolHashUniv
  , Dom rewards :=: Dom stakeDeposits
  , Dom stakeDeposits :⊆: credsUniv
  , Dom delegations :⊆: credsUniv
  , Dom delegations :⊆: Dom rewards
  , Dom regPools :=: Dom mockPoolDistr
  , Dom regPools :=: Dom poolDeposits
  , Dom regPools :⊆: poolHashUniv
  , Dom retiring :⊆: Dom regPools
  , SumsTo (Right (Coin 1)) deposits EQL [SumMap stakeDeposits, SumMap poolDeposits]
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [SumMap mockPoolDistr]
  , SumsTo
      (Right (Coin 1))
      totalAda
      EQL
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
  putStrLn "testing: find a viable order of variables"
  when loud $ putStrLn "======================================================="
  case runTyped (compile standardOrderInfo $ constraints Shelley) of
    Right x ->
      if loud
        then print x
        else putStrLn "+++ OK, passed 1 test."
    Left xs -> error (unlines xs)

-- | Test that we can compute a solution
test7 :: Bool -> IO ()
test7 loud = do
  putStrLn "testing: compute a solution"
  when loud $ putStrLn "======================================================="
  let proof = Shelley
  when loud $ putStrLn (show $ constraints proof)
  graph <- monadTyped $ compile standardOrderInfo $ constraints proof
  when loud $ putStrLn (show graph)
  result <- generate (genDependGraph loud proof graph)
  subst <- case result of
    Left msgs -> error (unlines msgs)
    Right x -> pure x
  when loud $ putStrLn (show subst)
  env <- monadTyped $ substToEnv subst emptyEnv
  ss <- monadTyped (mapM (makeTest env) $ constraints proof)
  let bad = filter (\(_, b, _) -> not b) ss
  if not (null bad)
    then error ("Some conditions fail\n" ++ unlines (map (\(s, _, _) -> s) bad))
    else putStrLn "+++ OK, passed 1 test."

-- ================================================

pstateConstraints :: Era era => [Pred era]
pstateConstraints =
  [ Sized (ExactSize 20) poolHashUniv
  , -- we have , retiring :⊆: regPools        :⊆: poolsUinv AND
    --                         futureRegPools) :⊆: poolHashUniv
    -- We need to adjust the sizes so that these constraints are possible
    Sized (AtMost 3) (Dom futureRegPools) -- Small enough that its leaves some slack with poolHashUniv
  , Sized (Range 5 6) (Dom retiring) -- Small enough that its leaves some slack with poolHashUniv
  , Sized (AtLeast 9) (Dom regPools) -- Large enough that retiring is a strict subset
  -- the disjoint set futureRegPools can be as big as three
  -- the must set can be as big as 6
  -- so the may set can't be bigger than 6
  -- or we get an error like:   Size inconsistency. We need 7. The most we can get from (sub-disj) is 6
  , (Dom regPools) :⊆: poolHashUniv
  , (Dom poolDeposits) :⊆: poolHashUniv
  , (Dom regPools) :=: (Dom poolDeposits)
  , (Dom retiring) :⊆: (Dom regPools)
  , (Dom futureRegPools) :⊆: poolHashUniv
  , Disjoint (Dom futureRegPools) (Dom retiring)
  ]

test8 :: Gen Property
test8 =
  testn
    Alonzo
    "Test 8. Pstate constraints"
    False
    -- (stoi{setBeforeSubset = False})  -- Both of these choices work
    (stoi {setBeforeSubset = True}) -- But neither works without the Sized constraints
    pstateConstraints
    (Assemble pstateT)

-- ==============================================================
-- Test the summation predicates

sumPreds :: Reflect era => Proof era -> [Pred era]
sumPreds proof =
  [ Random totalAda
  , fees :=: (Lit CoinR (Coin 400))
  , deposits :=: (Lit CoinR (Coin 200))
  , SumsTo (Right (Coin 1)) totalAda LTE [One fees, One deposits, One utxoAmt]
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL poolDistr]
  , SumsTo (Right (Coin 1)) utxoAmt GTH [ProjMap CoinR outputCoinL (utxo proof)]
  , Sized (AtLeast 1) (utxo proof) -- Any map that is summed, to a nonzero amount, can't be empty.
  , Sized (AtLeast 1) poolDistr
  ]
  where
    utxoAmt = Var (V "utxoAmt" CoinR No)

test9 :: Gen Property
test9 = testn Alonzo "Test 9. Test of summing" False stoi (sumPreds Alonzo) Skip

test10 :: Gen Property
test10 =
  testn
    proof
    "Test 10. Test conditions EQL LTH LTE GTH GTE"
    False
    stoi
    [ Random x
    , Sized (AtLeast 2) y -- We Shouldn't need this, but without it breaks the LTH case  ? < 2 can't be solved
    , SumsTo (Right (Coin 1)) tsumLTH LTH [One x, One y]
    , SumsTo (Right (Coin 1)) tsumLTE LTE [One x, One y]
    , SumsTo (Right (Coin 1)) tsumGTH GTH [One x, One y]
    , SumsTo (Right (Coin 1)) tsumGTE GTE [One x, One y]
    , SumsTo (Right (Coin 1)) tsumEQL EQL [One x, One y]
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
    proof = Mary

test11 :: Gen Property
test11 =
  testn
    proof
    "Test 11. Instanaeous Sum Tests"
    False
    stoi
    [ Sized (AtLeast 1) treasury
    , Random instanTreasury
    , Sized (AtLeast 1) instanReserves
    , Negate (deltaReserves) :=: deltaTreasury
    , SumsTo (Right (Coin 1)) instanReservesSum EQL [SumMap instanReserves]
    , SumsTo (Right (DeltaCoin 1)) (Delta instanReservesSum) LTH [One (Delta reserves), One deltaReserves]
    , SumsTo (Right (Coin 1)) instanTreasurySum EQL [SumMap instanTreasury]
    , SumsTo (Right (DeltaCoin 1)) (Delta instanTreasurySum) LTH [One (Delta treasury), One deltaTreasury]
    , Before reserves deltaReserves
    ]
    Skip
  where
    proof = Alonzo

-- =====================================================

test12 :: Gen Property
test12 =
  testn
    p
    "Test 12. CanFollow tests"
    False
    stoi
    [ Random (pparams p)
    , Random (prevProtVer p)
    , (protVer p) `CanFollow` (prevProtVer p)
    , Component (Right (pparams p)) [field pp (protVer p)]
    , Component (Right (prevPParams p)) [field pp (prevProtVer p)]
    ]
    Skip
  where
    pp = PParamsR p
    p = Shelley

-- ==============================================================
-- Test the Component Predicate

componentPreds :: EraGov era => Proof era -> [Pred era]
componentPreds proof =
  [ Random (minFeeA proof)
  , Random size
  , Sized size llx -- Note since size denotes a range, llx and mm could have different sizes
  , Sized size mm
  , SumsTo
      (Right (Coin 1))
      (Lit CoinR (Coin 100))
      EQL
      [ One (minFeeA proof)
      , One (minFeeB proof)
      ]
  , Component (Right (pparams proof)) [field pp (minFeeA proof), field pp (minFeeB proof)]
  ]
  where
    pp = PParamsR proof
    llx = Var (V "llx" (SetR IntR) No)
    size = Var (V "size" SizeR No)
    mm = Var (V "mm" (MapR IntR IntR) No)

test13 :: Gen Property
test13 =
  testn
    proof
    "Test 13. Component tests"
    False
    stoi
    (componentPreds proof)
    Skip
  where
    proof = Shelley

-- ==============================================

test14 :: IO ()
test14 =
  failn
    Allegra
    "Test 14. Catch unsolveable use of Sized"
    False
    stoi
    [ Sized (ExactSize 6) credsUniv
    , Sized (ExactSize 10) rewards
    , Dom rewards :=: credsUniv
    ]
    Skip

-- ==============================================

test15 :: Gen Property
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
      SumsTo (Right 1) (maxBBSize proof) LTE [One (maxBHSize proof), One (maxTxSize proof)]
    , Component
        (Right (pparams proof))
        [field pp (maxTxSize proof), field pp (maxBHSize proof), field pp (maxBBSize proof)]
    ]
    Skip
  where
    proof = Alonzo
    pp = PParamsR proof

-- ============================================================

preds16 :: Era era => Proof era -> [Pred era]
preds16 _proof =
  [ Sized (ExactSize 6) poolHashUniv
  , Sized (Range 1 3) (Dom poolDistr) -- At least 1 but smaller than the poolHashUniv
  , Dom poolDistr :⊆: poolHashUniv
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL poolDistr]
  , Sized (Range 1 4) (Dom foox) -- At least 1 but smaller than the poolHashUniv
  , Dom foox :⊆: poolHashUniv
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [SumMap foox]
  ]
  where
    foox = Var (V "foo" (MapR PoolHashR RationalR) No)

test16 :: Gen Property
test16 =
  testn
    proof
    "Test 11C. Test NonEmpty subset"
    False
    stoi
    (preds16 proof)
    Skip
  where
    proof = Alonzo

-- ==============================================

univPreds :: Era era => Proof era -> [Pred era]
univPreds p =
  [ Sized (ExactSize 15) credsUniv
  , Sized (ExactSize 20) poolHashUniv
  , Sized (ExactSize 12) genesisHashUniv
  , Sized (ExactSize 12) txinUniv
  , Dom poolDistr :⊆: poolHashUniv
  , Dom regPools :⊆: poolHashUniv
  , Dom retiring :⊆: poolHashUniv
  , Dom futureRegPools :⊆: poolHashUniv
  , Dom poolDeposits :⊆: poolHashUniv
  , Dom prevBlocksMade :⊆: poolHashUniv
  , Dom currBlocksMade :⊆: poolHashUniv
  , Dom markPools :⊆: poolHashUniv
  , Dom markPoolDistr :⊆: poolHashUniv
  , Dom setPools :⊆: poolHashUniv
  , Dom goPools :⊆: poolHashUniv
  , Dom stakeDeposits :⊆: credsUniv
  , Dom delegations :⊆: credsUniv
  , Dom rewards :⊆: credsUniv
  , Dom markStake :⊆: credsUniv
  , Dom markDelegs :⊆: credsUniv
  , Dom setStake :⊆: credsUniv
  , Dom setDelegs :⊆: credsUniv
  , Dom goStake :⊆: credsUniv
  , Dom goDelegs :⊆: credsUniv
  , Dom instanReserves :⊆: credsUniv
  , Dom instanTreasury :⊆: credsUniv
  , Dom (pparamProposals p) :⊆: Dom genesisHashUniv
  , Dom (futurePParamProposals p) :⊆: Dom genesisHashUniv
  , Dom genDelegs :⊆: Dom genesisHashUniv
  , Dom (utxo p) :⊆: txinUniv
  ]

pstatePreds :: Era era => Proof era -> [Pred era]
pstatePreds _p =
  [ Sized (AtMost 3) (Dom futureRegPools) -- See comments in test8 why we need these Fixed predicates
  , Sized (Range 5 6) (Dom retiring) -- we need       retiring :⊆: regPools        :⊆: poolsUinv
  , Sized (AtLeast 9) (Dom regPools) -- AND we need                 futureRegPools  :⊆: poolHashUniv
  , Dom regPools :=: Dom poolDistr
  , Dom regPools :=: Dom poolDeposits
  , Dom retiring :⊆: Dom regPools
  , -- , Dom futureRegPools :⊆: Dom poolDistr  -- Don't think we want this
    Disjoint (Dom futureRegPools) (Dom retiring)
  ]

dstatePreds :: Era era => Proof era -> [Pred era]
dstatePreds _p =
  [ Sized (AtMost 8) rewards -- Small enough that its leaves some slack with credUniv
  , Dom rewards :=: Dom stakeDeposits
  , Dom delegations :⊆: Dom rewards
  , Random currentDRepState
  , Random drepDelegation
  , Random currentDRepState
  , Random committeeState
  , Random numDormantEpochs
  , Dom rewards :=: Rng ptrs
  , -- This implies (Fixed (ExactSize 3) instanReserves)
    -- But it also implies that the new introduced variable instanReservesDom also has size 3
    -- ,  Sized (ExactSize 3) (Dom instanReserves)
    -- , Sized (ExactSize 2) (Dom instanTreasury)

    Sized (AtLeast 1) treasury
  , Random instanTreasury
  , Sized (AtLeast 1) instanReserves
  , Negate (deltaReserves) :=: deltaTreasury
  , SumsTo (Right (Coin 1)) instanReservesSum EQL [SumMap instanReserves]
  , SumsTo (Right (DeltaCoin 1)) (Delta instanReservesSum) LTH [One (Delta reserves), One deltaReserves]
  , SumsTo (Right (Coin 1)) instanTreasurySum EQL [SumMap instanTreasury]
  , SumsTo (Right (DeltaCoin 1)) (Delta instanTreasurySum) LTH [One (Delta treasury), One deltaTreasury]
  , -- , Before reserves deltaReserves -- XXX
    -- , Before treasury deltaTreasury -- XXX
    ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]

accountstatePreds :: Proof era -> [Pred era]
accountstatePreds _p = [] -- Constraints on reserves and treasury appear in dstatePreds

utxostatePreds :: Reflect era => Proof era -> [Pred era]
utxostatePreds proof =
  [ SumsTo (Right (Coin 1)) utxoCoin EQL [ProjMap CoinR outputCoinL (utxo proof)]
  , SumsTo (Right (Coin 1)) deposits EQL [SumMap stakeDeposits, SumMap poolDeposits]
  , SumsTo
      (Right (Coin 1))
      totalAda
      EQL
      [One utxoCoin, One treasury, One reserves, One fees, One deposits, SumMap rewards]
  , Random fees
  , Random (pparamProposals proof)
  , Random (futurePParamProposals proof)
  , Random (futurePParams proof)
  ]

epochstatePreds :: EraGov era => Proof era -> [Pred era]
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
  , Random (prevPParams proof)
  , Random (currPParams proof)
  , Random donation
  , Sized (AtLeast 1) (maxBHSize proof)
  , Sized (AtLeast 1) (maxTxSize proof)
  , -- , Random (maxBBSize proof) -- This will cause underflow on Natural
    SumsTo
      (Right (1 % 1000))
      (Lit RationalR 1)
      EQL
      [ProjMap RationalR individualPoolStakeL markPoolDistr]
  , SumsTo (Right 1) (maxBBSize proof) LTE [One (maxBHSize proof), One (maxTxSize proof)]
  , Component
      (Right (pparams proof))
      [field pp (maxTxSize proof), field pp (maxBHSize proof), field pp (maxBBSize proof)]
  ]
  where
    pp = PParamsR proof

newepochstatePreds :: Era era => Proof era -> [Pred era]
newepochstatePreds _proof =
  [ Random currentEpoch
  , Sized (ExactSize 8) (Dom prevBlocksMade) -- Both prevBlocksMade and prevBlocksMadeDom will have size 8
  , Sized (ExactSize 8) (Dom currBlocksMade)
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL poolDistr]
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

test17 :: Gen Property
test17 =
  testn
    proof
    "Test 17. Full NewEpochState"
    False
    (stoi {sumBeforeParts = False})
    (newepochConstraints proof)
    (Assemble (newEpochStateT proof))
  where
    proof = Alonzo

-- ==========================================================
-- Tests of the Term projection function ProjS

projPreds1 :: Era era => Proof era -> [Pred era]
projPreds1 _proof =
  [ Sized (ExactSize 4) futureGenDelegs
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]

projPreds2 :: Era era => Proof era -> [Pred era]
projPreds2 _proof =
  [ Dom genDelegs :⊆: Dom genesisHashUniv
  , Sized (ExactSize 12) futGDUniv
  , Sized (ExactSize 6) (Dom genesisHashUniv)
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  ]
  where
    futGDUniv = (Var (V "futGDUniv" (SetR FutureGenDelegR) No))

test18a :: Gen Property
test18a = testn proof "Test 18a. Projection test" False stoi (projPreds1 proof) Skip
  where
    proof = Alonzo

test18b :: Gen Property
test18b =
  testn
    proof
    "Test 18b. Projection test with subset"
    False
    stoi
    (projPreds2 proof)
    (Assemble (Constr "Pair" (,) ^$ futureGenDelegs ^$ genDelegs))
  where
    proof = Alonzo

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
  putStrLn "testing: Test 19. test of projOnDom function"
  ans <- generate (help19 Mary)
  putStrLn (synopsis (MapR (FutureGenDelegR @TT) GenDelegPairR) ans) -- (ppMap pcFutureGenDeleg pcGenDelegPair ans))
  putStrLn "+++ OK, passed 1 test"

-- ===================================================

preds20 :: Era era => Proof era -> [Pred era]
preds20 proof =
  [ Sized (ExactSize 10) intUniv
  , Sized (AtMost 6) rewardsx
  , Dom rewardsx :⊆: intUniv
  , Dom rewardsx :=: Rng ptrsx
  ]
  where
    rewardsx = Var (pV proof "rewards" (MapR IntR Word64R) No)
    ptrsx = Var (pV proof "ptrs" (MapR PtrR IntR) No)
    intUniv = Var (pV proof "intUniv" (SetR IntR) No)

test20 :: Gen Property
test20 =
  testn
    proof
    "Test 20. Test ptr rewards iso"
    False
    stoi
    (preds20 proof)
    Skip
  where
    proof = Alonzo

test21 :: Int -> IO Bool
test21 seed = do
  let proof = Babbage
      w = Var (V "w" (ListR IntR) No)
      a = Var (V "a" IntR No)
      b = Var (V "b" IntR No)
      c = Var (V "c" IntR No)
      pred21 =
        Oneof
          w
          [
            ( 1
            , Invert "three" (typeRep @[Int]) (\x y z -> [x, y, z])
                :$ Partial a (\case [x, _, _] -> Just x; _ -> Nothing)
                :$ Partial b (\case [_, y, _] -> Just y; _ -> Nothing)
                :$ Partial c (\case [_, _, z] -> Just z; _ -> Nothing)
            , [Random a, Random b, Random c]
            )
          ,
            ( 1
            , Invert "two" (typeRep @[Int]) (\x y -> [x, y])
                :$ Partial a (\case [x, _] -> Just x; _ -> Nothing)
                :$ Partial b (\case [_, y] -> Just y; _ -> Nothing)
            , [Random a, Random b]
            )
          ,
            ( 1
            , Invert "one" (typeRep @[Int]) (\x -> [x])
                :$ Partial a (\case [x] -> Just x; _ -> Nothing)
            , [Random a]
            )
          ]
  env <-
    generateWithSeed
      seed
      ( pure emptySubst
          >>= toolChainSub proof standardOrderInfo [pred21]
          >>= (\subst -> monadTyped (substToEnv subst emptyEnv))
      )
  print env
  monadTyped $ runPred env pred21

-- ===================================================
-- run all the tests

testAll :: IO ()
testAll = defaultMain allExampleTests

allExampleTests :: TestTree
allExampleTests =
  testGroup
    "Example tests"
    [ testCase "test 1A. Cyclic preds detected" $
        case runTyped (compile @Babbage standardOrderInfo cyclicPred) of
          Right _ -> assertFailure ("Cyclic preds Not detected.")
          Left _ -> pure ()
    , testCase "test 1B. ACyclic preds solved" $
        case runTyped (compile @Babbage standardOrderInfo aCyclicPred) of
          Right _ -> pure ()
          Left xs -> assertFailure (unlines xs)
    , testIO
        "test 19 Test of projOnDom function"
        (generate (help19 Mary))
    , testIO "Test 4. Inconsistent Size" test4
    , testIO "Test 5. Bad Sum, impossible partition." test5
    , testIO "Test6. Find a viable order of variables" (test6 False)
    , testIO "Test7. Compute a solution" (test7 False)
    , testIO "Test 14 Catch unsolveable use of Sized" test14
    , testPropMax 30 "Test 3. PState example" test3
    , testPropMax 30 "Test 8. Pstate constraints" test8
    , testPropMax 30 "Test 9. Test of summing" test9
    , testPropMax 30 "Test 10. Test conditions EQL LTH LTE GTH GTE" test10
    , testPropMax 100 "Test 11. Instanaeous Sum Tests" test11
    , testPropMax 30 "Test 12. CanFollow tests" test12
    , testPropMax 30 "Test 13. Component tests" test13
    , testPropMax 30 "Test 15. Summation on Natural" test15
    , testPropMax 30 "Test 16. Test NonEmpty subset" test16
    , testPropMax 30 "Test 17. Full NewEpochState" (fmap (withMaxSuccess 30) test17)
    , testPropMax 30 "Test 18a. Projection test" test18a
    , testPropMax 30 "Test 18b. Projection test" test18b
    , testPropMax 30 "Test 20. ptr & rewards are inverses" test20
    , testPropMax 30 "Constraint soundness" $ within 1000000 $ prop_soundness
    , testProperty "Shrinking soundness" $ withMaxSuccess 30 $ prop_shrinking
    , testProperty "NewEpochState and Tx generation" $ do
        (st, tx, _) <- genTxAndNewEpoch def $ Conway
        (st, tx) `deepseq` pure True
    , testPreds "ListWhereTest" Conway listWherePreds
    ]

-- ==============================

-- | Use to generate a solution to 'preds', test that the
--   solution meets the 'preds', and puts you in the Repl to
--   inspect the value of each variable solved for.
--   Companion function to 'testPreds'
demoPreds :: Reflect era => Proof era -> [Pred era] -> IO ()
demoPreds proof preds = do
  let tryPred env p = (p, errorTyped (runPred env p))
  env <-
    generate
      ( pure emptySubst
          >>= toolChainSub proof standardOrderInfo preds
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  let pairs = map (tryPred env) preds
  mapM_ (\(p, b) -> putStrLn (show p ++ " = " ++ show b)) pairs
  modeRepl Interactive proof env ""

-- | Use this to generate a solution to 'preds', and make a TestTree
--   that tests the solution meets the 'preds'.
--   Companion function to 'demoPreds'
testPreds :: Reflect era => String -> Proof era -> [Pred era] -> TestTree
testPreds name proof preds = do
  let tryPred env p = (p, errorTyped (runPred env p))
  testProperty name $ do
    env <-
      ( pure emptySubst
          >>= toolChainSub proof standardOrderInfo preds
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
        )
    let pairs = map (tryPred env) preds
    pure (property (and (map snd pairs)))

listWherePreds :: [Pred (ConwayEra StandardCrypto)]
listWherePreds =
  [ Sized (ExactSize 10) credsUniv
  , Sized (ExactSize 10) voteUniv
  , ListWhere
      (Range 2 4)
      ans
      (pairT CredR VCredR)
      [Member (Right (pair1 CredR)) credsUniv, Member (Right (pair2 VCredR)) voteUniv]
  ]
  where
    ans = Var (V "ans" (ListR (PairR CredR VCredR)) No)

mainListWhere :: IO ()
mainListWhere = demoPreds Conway listWherePreds

govPreds :: [Pred (ConwayEra StandardCrypto)]
govPreds =
  [ ListWhere
      (Range 2 4)
      currGovStates
      govActionStateTarget
      [ Random hotCommitteeCredsUniv
      , Random voteUniv
      , Random poolHashUniv
      , Lit CoinR (Coin 13) :=: depositV
      , Lit EpochR (EpochNo 3) :=: currentEpoch
      , Random idV
      , Subset (Dom committeeVotesV) hotCommitteeCredsUniv
      , Subset (Dom drepVotesV) voteUniv
      , Subset (Dom stakePoolVotesV) poolHashUniv
      , proposalDeposits :=: depositV
      , Random returnAddrV
      , Random actionV
      , currentEpoch :=: proposedInV
      , Random expiresAfterV
      , Sized (Range 0 3) childrenV
      ]
  ]

mainGov :: IO ()
mainGov = demoPreds Conway govPreds
