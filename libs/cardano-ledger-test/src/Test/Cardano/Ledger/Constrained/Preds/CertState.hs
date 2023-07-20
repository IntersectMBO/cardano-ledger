{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Constrained.Preds.CertState where

import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Era (EraCrypto)
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.Pretty (ppMap)
import Cardano.Ledger.Shelley.LedgerState (availableAfterMIR)
import Cardano.Ledger.Shelley.TxCert (MIRPot (..))
import GHC.Real ((%))
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL)
import Test.Cardano.Ledger.Constrained.Monad (generateWithSeed, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.Repl (goRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..), genFromSize)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (
  PrettyC (..),
  pcDState,
  pcIndividualPoolStake,
  pcKeyHash,
  pcPState,
  pcVState,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck

-- | A good spread of Coins with at least one (Coin 0)
manyCoin :: Size -> Gen [Coin]
manyCoin size = do
  n <- genFromSize size
  (Coin 0 :) <$> vectorOf (n - 1) variedCoin

-- ======================================

vstatePreds :: Proof era -> [Pred era]
vstatePreds _p =
  [ Sized (Range 3 8) dreps
  , Sized (Range 5 7) (Dom ccHotKeys)
  , Subset dreps voteUniv
  , Subset (Dom ccHotKeys) voteHashUniv
  ]

vstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
vstateStage proof = toolChainSub proof standardOrderInfo (vstatePreds proof)

mainV :: IO ()
mainV = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure emptySubst
          >>= universeStage proof
          >>= vstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  vstate <- monadTyped $ runTarget env vstateT
  putStrLn (show (pcVState vstate))
  putStrLn "\n"
  putStrLn (unlines (otherFromEnv [] env))

-- ==========================================

pstateNames :: [String]
pstateNames =
  [ "regPools"
  , "futureRegPools"
  , "retiring"
  , "poolDeposits"
  ]

pstatePreds :: Proof era -> [Pred era]
pstatePreds _p =
  [ -- These Sized constraints are needd to be ensure that regPools is bigger than retiring
    Sized (ExactSize 3) retiring
  , Sized (AtLeast 3) regPools
  , Subset (Dom regPools) poolHashUniv
  , Subset (Dom futureRegPools) poolHashUniv
  , Subset (Dom poolDeposits) poolHashUniv
  , Subset (Dom retiring) (Dom regPools) -- Note regPools must be bigger than retiring
  , Dom regPools :=: Dom poolDeposits
  , NotMember (Lit CoinR (Coin 0)) (Rng poolDeposits)
  , Disjoint (Dom regPools) (Dom futureRegPools)
  , epochs :=: Elems retiring
  , Choose
      (ExactSize 3)
      epochs
      [ (1, Constr "id" id ^$ e, [CanFollow e currentEpoch])
      , (1, Constr "(+1)" (+ 1) ^$ e, [CanFollow e currentEpoch])
      , (1, Constr "(+3)" (+ 3) ^$ e, [CanFollow e currentEpoch])
      , (1, Constr "(+5)" (+ 5) ^$ e, [CanFollow e currentEpoch])
      ]
  , -- poolDistr not needed in PState, but is needed in NewEpochState
    -- But since it is so intimately tied to regPools we define it here
    -- Alternately we could put this in NewEpochState, and insist that pStateStage
    -- preceed newEpochStateStage
    Dom regPools :=: Dom poolDistr
  , SumsTo (Right (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL poolDistr]
  ]
  where
    e = var "e" EpochR
    epochs = var "epochs" (ListR EpochR)

pstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
pstateStage proof = toolChainSub proof standardOrderInfo (pstatePreds proof)

mainP :: IO ()
mainP = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure emptySubst
          >>= universeStage proof
          >>= pstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  pstate <- monadTyped $ runTarget env pstateT
  pDistr <- monadTyped (findVar (unVar poolDistr) env)
  putStrLn (show (pcPState pstate))
  putStrLn "\n"
  putStrLn (show (ppMap pcKeyHash pcIndividualPoolStake pDistr))
  putStrLn "\n"
  putStrLn (unlines (otherFromEnv [] env))

-- =================================================
-- A Field used to compute 'genDelegs'

-- | A field that selects the 'genDelegKeyHash' field from a 'GenDelegPair'
--   It also silently casts the 'KeyRole. from 'Genesis to 'Witness
gdKeyHashField ::
  Field
    era
    (GenDelegPair (EraCrypto era))
    (KeyHash 'Witness (EraCrypto era))
gdKeyHashField =
  Field
    "gdKeyHash1"
    WitHashR
    GenDelegPairR
    ( lens
        (\(GenDelegPair x _) -> asWitness x)
        (\(GenDelegPair _ y) x -> GenDelegPair (coerceKeyRole x) y)
    )

-- | A Var Term that pairs the Field 'gdKeyHashField'
gdKeyHash :: Term era (KeyHash 'Witness (EraCrypto era))
gdKeyHash = fieldToTerm gdKeyHashField

gdkeyL :: Lens' (GenDelegPair c) (KeyHash 'Witness c)
gdkeyL =
  ( lens
      (\(GenDelegPair x _) -> asWitness x)
      (\(GenDelegPair _ y) x -> GenDelegPair (coerceKeyRole x) y)
  )

-- ============================================================================

certStatePreds :: Proof era -> [Pred era]
certStatePreds _p =
  [ MetaSize (SzExact (fromIntegral (quorumConstant + 2))) genDelegSize
  , --  , GenFrom quorum (constTarget (pure (fromIntegral quorumConstant)))

    -- These really belong in the AccountState in the EpochState, But they are the only pieces of the EpochState we use.
    -- and are necessary to compute the instantaneous rewards, which are in the DState.
    GenFrom treasury (constTarget (Coin <$> choose (1000, 4000)))
  , GenFrom reserves (constTarget (Coin <$> choose (4000, 5000)))
  , GenFrom deltaTreasury (constTarget (DeltaCoin <$> choose (-200, 400)))
  , Negate (deltaReserves) :=: deltaTreasury -- Means deltaReserves ranges from (-400,200)
  , -- 'rewards' needs to be small enough that it leaves some slack with
    -- credUniv (size about 30), but it also cannot be empty
    MetaSize (SzRng 8 15) rewardSize
  , Sized rewardSize rewards
  , -- If 'instanReserves' or 'instanTreasury' have size zero, the SumsTo can't be solved
    Sized (AtLeast 1) instanReserves
  , Sized (AtLeast 1) instanTreasury
  , Dom rewards :⊆: credsUniv
  , GenFrom rewardRange (Constr "many" manyCoin ^$ rewardSize)
  , rewardRange :=: Elems rewards
  , NotMember (Lit CoinR (Coin 0)) (Rng stakeDeposits)
  , Dom rewards :=: Dom stakeDeposits
  , Dom delegations :⊆: Dom rewards
  , Dom rewards :=: Rng ptrs
  , -- Preds to compute genDelegs
    -- First, a set of GenDelegPairs, where no keyHash is repeated.
    Sized genDelegSize gdKeyHashSet
  , ProjS gdkeyL WitHashR gdKeyHashSet `Subset` (Dom keymapUniv)
  , gdKeyHashList :<-: setToListTarget gdKeyHashSet
  , Sized genDelegSize genDelegs
  , gdKeyHashList :=: Elems genDelegs
  , Dom genDelegs :⊆: Dom genesisHashUniv
  , Dom instanReserves :⊆: credsUniv
  , SumsTo (Left (Coin 1)) instanReservesSum EQL [SumMap instanReserves]
  , SumsTo
      (Right (DeltaCoin 1))
      (Delta instanReservesSum)
      LTH
      [One (Delta reserves), One deltaReserves, One (Lit DeltaCoinR (DeltaCoin (-3000)))]
  , Dom instanTreasury :⊆: credsUniv
  , NotMember (Lit CoinR (Coin 0)) (Rng instanTreasury)
  , SumsTo (Right (Coin 1)) instanTreasurySum EQL [SumMap instanTreasury]
  , SumsTo (Right (DeltaCoin 1)) (Delta instanTreasurySum) LTH [One (Delta treasury), One deltaTreasury]
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  , mirAvailTreasury :<-: (Constr "computeAvailTreasury" (availableAfterMIR TreasuryMIR) :$ accountStateT :$ instantaneousRewardsT)
  , mirAvailReserves :<-: (Constr "computeAvailReserves" (availableAfterMIR ReservesMIR) :$ accountStateT :$ instantaneousRewardsT)
  ]
  where
    rewardSize = var "rewardSize" SizeR
    rewardRange = var "rewardRange" (ListR CoinR)
    genDelegSize = var "genDelegSize" SizeR
    gdKeyHashSet = var "gdKeyHashSet" (SetR GenDelegPairR)
    gdKeyHashList = var "gdKeyHashList" (ListR GenDelegPairR)

dstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
dstateStage proof = toolChainSub proof standardOrderInfo (certStatePreds proof)

mainD :: Int -> IO ()
mainD seed = do
  let proof = Babbage Standard
  env <-
    generateWithSeed
      seed
      ( pure emptySubst
          >>= universeStage proof
          >>= dstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  dState <- monadTyped $ runTarget env dstateT
  putStrLn (show (pcDState dState))
  goRepl proof env ""

-- ===============================================

mainC :: IO ()
mainC = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure emptySubst
          >>= universeStage proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  certState <- monadTyped $ runTarget env certstateT
  putStrLn (show (prettyC proof certState))
  goRepl proof env ""
