{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Ledger.Constrained.Tests where

import Data.Map (Map)
import qualified Data.Map as Map

import Cardano.Ledger.DPState (PState (..))
import Cardano.Ledger.Pretty
import Test.Cardano.Ledger.Constrained.Ast

import Cardano.Ledger.Coin (Coin (..))
import Test.Cardano.Ledger.Constrained.Env
import Cardano.Ledger.Era (Era (EraCrypto))
import Cardano.Ledger.Shelley
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Cardano.Ledger.Constrained.Classes
import Test.Cardano.Ledger.Constrained.Monad
import Test.Cardano.Ledger.Constrained.Spec
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Proof (
  Evidence (..),
  Proof (..),
  Reflect (..),
 )
import Test.QuickCheck hiding (Fixed, total)

-- Generators ---

genLiteral :: Era era => Rep era t -> Gen (Literal era t)
genLiteral rep = Lit rep <$> genRep rep

genSizedLiteral :: Era era => Int -> Rep era t -> Gen (Literal era t)
genSizedLiteral n rep = Lit rep <$> genSizedRep n rep

genFreshVarName :: Env era -> Gen String
genFreshVarName (Env env) = elements varNames
  where
    varNames = take 10 [ name | s <- "" : varNames
                              , c <- ['A'..'Z']
                              , let name = s ++ [c]
                              , Map.notMember name env
                       ]

envVarsOfType :: Env era -> Rep era t -> [(V era t, Literal era t)]
envVarsOfType (Env env) rep = concatMap (wellTyped rep) $ Map.toList env
  where
    wellTyped :: Rep era t -> (String, Payload era) -> [(V era t, Literal era t)]
    wellTyped rep (name, Payload rep' val access) =
      case testEql rep rep' of
        Just Refl -> [(V name rep access, Lit rep val)]
        Nothing   -> []

genTerm :: Era era => Env era -> Rep era t -> Bool -> Gen (Term era t, Env era)
genTerm env rep allowFixed = sized $ genSizedTerm env rep allowFixed

genSizedTerm :: Era era => Env era -> Rep era t -> Bool -> Int -> Gen (Term era t, Env era)
genSizedTerm env rep allowFixed size = frequency $
  [ (5, genFixed)       | allowFixed ] ++
  [ (5, genExistingVar) | not $ null existingVars ] ++
  [ (1, genFreshVar)    | size > 0 || not allowFixed ]
  where
    existingVars = envVarsOfType env rep

    genFixed       = (, env) . Fixed <$> genSizedLiteral size rep
    genExistingVar = (, env) . Var <$> elements (map fst existingVars)

    genFreshVar = do
      name      <- genFreshVarName env
      Lit _ val <- genSizedLiteral size rep
      let var = V name rep No
      pure (Var var, storeVar var val env)

data TypeInEra era where
  TypeInEra :: (Show t, Ord t) => Rep era t -> TypeInEra era

genType :: Gen (TypeInEra era)
genType = elements [TypeInEra IntR, TypeInEra (SetR IntR)]

genBaseType :: Gen (TypeInEra era)
genBaseType = elements [TypeInEra IntR]

errPred :: [String] -> Pred era
errPred errs = Fixed (Lit (ListR StringR) ["Errors:"]) :=: Fixed (Lit (ListR StringR) errs)

genPred :: Era era => Env era -> Gen (Pred era, Env era)
genPred env = sized $ genSizedPred env

genSizedPred :: Era era => Env era -> Int -> Gen (Pred era, Env era)
genSizedPred env size = frequency
  [ (1, sized) ]
  where
    -- Fixed size
    sized = do
      TypeInEra rep <- genBaseType
      (set, env')   <- genSizedTerm env (SetR rep) False size -- Must contain a variable!
      case runTyped $ runTerm env' set of
        Left errs -> pure (errPred errs, env')
        Right val -> pure (Sized n set, env')
          where n = Fixed $ Lit Word64R (getsize val)

genPreds :: Era era => Env era -> Gen ([Pred era], Env era)
genPreds env = do
  n <- choose (1, 10)
  loop (n :: Int) env
  where
    loop n env
      | n == 0    = pure ([], env)
      | otherwise = do
        (pr, env')   <- genPred env
        (prs, env'') <- loop (n - 1) env'
        pure (pr : prs, env'')

-- Tests ---

type TestEra = ShelleyEra C_Crypto

testProof :: Proof TestEra
testProof = Shelley Mock

testEnv :: Env TestEra
testEnv = Env $ Map.fromList [ ("A", Payload IntR 5 No) ]

ensureRight :: Testable prop => Either [String] a -> (a -> prop) -> Property
ensureRight (Left errs) _ = counterexample (unlines errs) False
ensureRight (Right x) prop = property $ prop x

ensureTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ensureTyped = ensureRight . runTyped

ifTyped :: Testable prop => Typed a -> (a -> prop) -> Property
ifTyped t prop =
  case runTyped t of
    Left{}  -> False ==> False
    Right x -> property $ prop x

-- | Generate a set of satisfiable constraints and check that we can generate a solution and that it
--   actually satisfies the constraints.
prop_soundness :: Property
prop_soundness =
  forAll (genPreds @TestEra emptyEnv)                        $ \ (preds, _) ->
  ensureTyped (compile standardOrderInfo preds)              $ \ graph ->
  forAll (genDependGraph testProof graph) . flip ensureRight $ \ subst ->
  let env = substToEnv subst emptyEnv
      checkPred pr = counterexample ("Failed: " ++ show pr) $ ensureTyped (runPred env pr) id
  in conjoin $ map checkPred preds

