{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.NewEpochState where

import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley.Core (EraGov)
import Control.Monad (when)
import Data.Default.Class (Default (def))
import Data.Ratio ((%))
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (dstateStage, pstateStage, vstateStage)
import Test.Cardano.Ledger.Constrained.Preds.LedgerState (ledgerStateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes (universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite (OrderInfo (..), standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Utils (testIO)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcEpochState, pcNewEpochState)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain)

-- ===========================================================

epochstatePreds :: EraGov era => Proof era -> [Pred era]
epochstatePreds _proof =
  [ Dom markStake `Subset` credsUniv
  , Dom markDelegs `Subset` credsUniv
  , Rng markDelegs `Subset` poolHashUniv
  , Sized (Range 1 2) markPools
  , Dom markPools `Subset` poolHashUniv
  , Dom setStake `Subset` credsUniv
  , Dom setDelegs `Subset` credsUniv
  , Rng setDelegs `Subset` poolHashUniv
  , Sized (Range 1 2) setPools
  , Dom setPools `Subset` poolHashUniv
  , Dom goStake `Subset` credsUniv
  , Dom goDelegs `Subset` credsUniv
  , Rng goDelegs `Subset` poolHashUniv
  , Sized (Range 1 2) goPools
  , Dom goPools `Subset` poolHashUniv
  , Random snapShotFee
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL markPoolDistr]
  , Sized (Range 2 6) markPoolDistr
  ]

newEpochStatePreds :: Era era => Proof era -> [Pred era]
newEpochStatePreds _proof =
  [ Sized (ExactSize 4) (Dom prevBlocksMade) -- Both prevBlocksMade and prevBlocksMadeDom will have size 4
  , Sized (ExactSize 4) (Dom currBlocksMade)
  , Dom prevBlocksMade `Subset` poolHashUniv
  , Dom currBlocksMade `Subset` poolHashUniv
  ]

-- ========================

epochStateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
epochStateStage proof = toolChainSub proof standardOrderInfo (epochstatePreds proof)

demoES :: Reflect era => Proof era -> ReplMode -> IO ()
demoES proof mode = do
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= ledgerStateStage def proof
          >>= epochStateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  epochstate <- monadTyped $ runTarget env (epochStateT proof)
  let env2 = getTarget epochstate (epochStateT proof) emptyEnv
  when (mode == Interactive) $ print (pcEpochState proof epochstate)
  modeRepl mode proof env2 ""

demoESTest :: TestTree
demoESTest = testIO "Testing EpochState Stage" (demoES (Conway Standard) CI)

mainES :: IO ()
mainES = defaultMain $ testIO "Testing EpochState Stage" (demoES (Conway Standard) Interactive)

-- ====================================================

newEpochStateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
newEpochStateStage proof = toolChainSub proof (standardOrderInfo {sumBeforeParts = False}) (newEpochStatePreds proof)

demoNES :: Reflect era => Proof era -> ReplMode -> IO ()
demoNES proof mode = do
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= ledgerStateStage def proof
          >>= epochStateStage proof
          >>= newEpochStateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  newepochstate <- monadTyped $ runTarget env (newEpochStateT proof)
  let env2 = getTarget newepochstate (newEpochStateT proof) emptyEnv
  when (mode == Interactive) $ putStrLn (show (pcNewEpochState proof newepochstate))
  modeRepl mode proof env2 ""

demoNESTest :: TestTree
demoNESTest = testIO "Testing NewEpochState Stage" (demoNES (Conway Standard) CI)

mainNES :: IO ()
mainNES = defaultMain $ testIO "Testing NewEpochState Stage" (demoNES (Conway Standard) Interactive)

-- ==========================
