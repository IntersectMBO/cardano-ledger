{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.CertState where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.DRep (drepAnchorL, drepDepositL, drepExpiryL)
import Cardano.Ledger.Era (Era, EraCrypto)
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.Shelley.LedgerState (availableAfterMIR)
import Cardano.Ledger.Shelley.TxCert (MIRPot (..))
import Control.Monad (when)
import Data.Default.Class (Default (def))
import qualified Data.Map as Map
import Data.Ratio ((%))
import Debug.Trace
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..))
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Examples (testIO)
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL, strictMaybeToMaybeL)
import Test.Cardano.Ledger.Constrained.Monad (generateWithSeed, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes hiding (demo, demoTest, main)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..), genFromSize)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.Functions (protocolVersion)
import Test.Cardano.Ledger.Generic.PrettyCore (
  pcCertState,
  pcDState,
  pcPState,
  pcVState,
 )
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)

-- =========================================================

-- | A good spread of Coins with at least one (Coin 0)
manyCoin :: Size -> Gen [Coin]
manyCoin size = do
  n <- genFromSize size
  (Coin 0 :) <$> vectorOf (n - 1) variedCoin

-- ======================================

-- | Predicates for Voting State, only work in Conway or later Eras.
vstatePreds :: forall era. Era era => Proof era -> [Pred era]
vstatePreds p = case whichPParams p of
  PParamsConwayToConway ->
    [ Sized (Range 10 20) currentDRepState
    , Sized (Range 5 7) (Dom committeeState)
    , ForEach
        (Range 25 25) -- It is possible we create duplicates, so we make a few more than 20 (the size of drepState)
        drepStateSet
        (Pat DRepStateR [Arg expire, Arg anchor, Arg deposit])
        [ Random (fieldToTerm anchor)
        , GenFrom (fieldToTerm expire) (Constr "+200To500" (\(EpochNo n) -> EpochNo <$> choose (n + 200, n + 500)) ^$ currentEpoch)
        , drepDeposit p :=: (fieldToTerm deposit)
        ]
    , Subset (Dom currentDRepState) voteUniv
    , Subset (Rng currentDRepState) drepStateSet
    , SumsTo (Right (Coin 1)) totalDRepDeposit EQL [ProjMap CoinR drepDepositL currentDRepState]
    , SumsTo (Left (Coin 1)) totalDRepDeposit EQL [SumList (Elems drepDeposits)]
    , Subset (Dom committeeState) voteCredUniv
    , Random numDormantEpochs
    ]
  _ ->
    [ Sized (Range 0 0) currentDRepState
    , Sized (Range 0 0) committeeState
    , Lit EpochR (EpochNo 0) :=: numDormantEpochs
    , Random currentDRepState
    ]
  where
    drepStateSet = Var $ pV p "drepStateSet" (SetR DRepStateR) No
    deposit = Field @era "deposit" CoinR DRepStateR drepDepositL
    totalDRepDeposit = Var $ pV p "totalDRepDeposit" CoinR No
    anchor = Field @era "anchor" (MaybeR AnchorR) DRepStateR (drepAnchorL . strictMaybeToMaybeL)
    expire = Field @era "expire" EpochR DRepStateR drepExpiryL

vstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
vstateStage proof = toolChainSub proof standardOrderInfo (vstatePreds proof)

demoV :: ReplMode -> IO ()
demoV mode = do
  let proof = Conway Standard
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= vstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  vstate <- monadTyped $ runTarget env vstateT
  when (mode == Interactive) $ putStrLn (show (pcVState vstate))
  modeRepl mode proof env ""

demoTestV :: TestTree
demoTestV = testIO "Testing VState Stage" (demoV CI)

mainV :: IO ()
mainV = defaultMain $ testIO "Testing VState Stage" (demoV Interactive)

-- ==========================================

pstateNames :: [String]
pstateNames =
  [ "regPools"
  , "futureRegPools"
  , "retiring"
  , "poolDeposits"
  ]

pstatePreds :: Era era => Proof era -> [Pred era]
pstatePreds p = pstateGenPreds p ++ pstateCheckPreds p

pstateGenPreds :: Era era => Proof era -> [Pred era]
pstateGenPreds _ =
  [ -- These Sized constraints are needd to be ensure that regPools is bigger than retiring
    Sized (ExactSize 5) retiring
  , Sized (AtLeast 20) regPools
  , Subset (Dom regPools) poolHashUniv
  , Sized (Range 10 15) futureRegPools
  , Subset (Dom futureRegPools) poolHashUniv
  , Subset (Dom poolDeposits) poolHashUniv
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

pstateCheckPreds :: Era era => Proof era -> [Pred era]
pstateCheckPreds _ =
  [ Subset (Dom retiring) (Dom regPools) -- Note regPools must be bigger than retiring
  , Dom regPools :=: Dom poolDeposits
  , NotMember (Lit CoinR (Coin 0)) (Rng poolDeposits)
  , Disjoint (Dom regPools) (Dom futureRegPools)
  ]

pstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
pstateStage proof = toolChainSub proof standardOrderInfo (pstatePreds proof)

demoP :: ReplMode -> IO ()
demoP mode = do
  let proof = Babbage Standard
  env <-
    generate
      ( pure emptySubst
          >>= universeStage def proof
          >>= pstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  pstate <- monadTyped $ runTarget env pstateT
  when (mode == Interactive) $ do
    putStrLn (show (pcPState pstate))
  modeRepl mode proof env ""

demoTestP :: TestTree
demoTestP = testIO "Testing PState Stage" (demoP CI)

mainP :: IO ()
mainP = defaultMain $ testIO "Testing PState Stage" (demoP Interactive)

-- =================================================
-- A Field used to compute 'genDelegs'

-- | A field that selects the 'genDelegKeyHash' field from a 'GenDelegPair'
--   It also silently casts the 'KeyRole. from 'Genesis to 'Witness
gdKeyHashField ::
  Era era =>
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
gdKeyHash :: Era era => Term era (KeyHash 'Witness (EraCrypto era))
gdKeyHash = fieldToTerm gdKeyHashField

gdkeyL :: Lens' (GenDelegPair c) (KeyHash 'Witness c)
gdkeyL =
  ( lens
      (\(GenDelegPair x _) -> asWitness x)
      (\(GenDelegPair _ y) x -> GenDelegPair (coerceKeyRole x) y)
  )

-- ============================================================================

certStatePreds :: Era era => Proof era -> [Pred era]
certStatePreds p = certStateGenPreds p ++ certStateCheckPreds p

certStateGenPreds :: Era era => Proof era -> [Pred era]
certStateGenPreds p =
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
  , if protocolVersion p >= protocolVersion (Conway Standard)
      then Sized (ExactSize 0) ptrs
      else Dom rewards :=: Rng ptrs
  , Dom drepDelegation :⊆: credsUniv
  , Rng drepDelegation :⊆: drepUniv
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
  , ProjS fGenDelegGenKeyHashL GenHashR (Dom futureGenDelegs) :=: Dom genDelegs
  , NotMember (Lit CoinR (Coin 0)) (Rng instanTreasury)
  , SumsTo (Right (Coin 1)) instanTreasurySum EQL [SumMap instanTreasury]
  , SumsTo (Right (DeltaCoin 1)) (Delta instanTreasurySum) LTH [One (Delta treasury), One deltaTreasury]
  , mirAvailTreasury :<-: (Constr "computeAvailTreasury" (availableAfterMIR TreasuryMIR) :$ Mask accountStateT :$ Mask instantaneousRewardsT)
  , mirAvailReserves :<-: (Constr "computeAvailReserves" (availableAfterMIR ReservesMIR) :$ Mask accountStateT :$ Mask instantaneousRewardsT)
  , Dom drepDelegation :⊆: credsUniv
  , Rng drepDelegation :⊆: drepUniv
  ]
  where
    rewardSize = var "rewardSize" SizeR
    rewardRange = var "rewardRange" (ListR CoinR)
    genDelegSize = var "genDelegSize" SizeR
    gdKeyHashSet = var "gdKeyHashSet" (SetR GenDelegPairR)
    gdKeyHashList = var "gdKeyHashList" (ListR GenDelegPairR)

certStateCheckPreds :: Era era => Proof era -> [Pred era]
certStateCheckPreds p =
  [ NotMember (Lit CoinR (Coin 0)) (Rng stakeDeposits)
  , Dom rewards :=: Dom stakeDeposits
  , Dom delegations :⊆: Dom rewards
  , if protocolVersion p >= protocolVersion (Conway Standard)
      then Sized (ExactSize 0) ptrs
      else Dom rewards :=: Rng ptrs
  ]

dstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
dstateStage proof = toolChainSub proof standardOrderInfo (certStatePreds proof)

demoD :: ReplMode -> Int -> IO ()
demoD mode seed = do
  let proof = Babbage Standard
  env <-
    generateWithSeed
      seed
      ( pure emptySubst
          >>= universeStage def proof
          >>= dstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  dState <- monadTyped $ runTarget env dstateT
  when (mode == Interactive) $ putStrLn (show (pcDState dState))
  modeRepl mode proof env ""

demoTestD :: TestTree
demoTestD = testIO "Testing DState Stage" (demoD CI 99)

mainD :: Int -> IO ()
mainD seed = defaultMain $ testIO "Testing DState Stage" (demoD Interactive seed)

-- ===============================================

tell :: String -> (Subst era -> a) -> (Subst era -> a)
tell s f (Subst m) =
  case Map.lookup "drepState" m of
    Nothing -> trace (s ++ " NO") (f (Subst m))
    Just x -> trace (s ++ " YES " ++ show x) (f (Subst m))

demoC :: ReplMode -> IO ()
demoC mode = do
  let proof = Conway Standard
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  certState <- monadTyped $ runTarget env certstateT
  when (mode == Interactive) $ putStrLn (show (pcCertState certState))
  modeRepl mode proof env ""

demoTestC :: TestTree
demoTestC = testIO "Testing CertState Stage" (demoC CI)

mainC :: IO ()
mainC = defaultMain $ testIO "Testing CertState Stage" (demoC Interactive)

demoTest :: TestTree
demoTest =
  testGroup
    "CertState tests"
    [demoTestV, demoTestP, demoTestD, demoTestC]
