{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.CertState where

import Cardano.Ledger.BaseTypes (EpochNo (..))
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Conway.State (ConwayEraCertState, EraCertState)
import Cardano.Ledger.Core (Era)
import Cardano.Ledger.DRep (drepAnchorL, drepDepositL, drepExpiryL)
import Cardano.Ledger.Keys (GenDelegPair (..), KeyHash, KeyRole (..), asWitness, coerceKeyRole)
import Cardano.Ledger.Shelley.LedgerState (availableAfterMIR)
import Cardano.Ledger.Shelley.TxCert (MIRPot (..))
import Control.Monad (when)
import Data.Default (Default (def))
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..), unCertStateF)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Lenses (fGenDelegGenKeyHashL, strictMaybeToMaybeL)
import Test.Cardano.Ledger.Constrained.Monad (generateWithSeed, monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.Universes hiding (demo, demoTest, main)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..), genFromSize)
import Test.Cardano.Ledger.Constrained.Solver
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Utils (testIO)
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

vstatePreds :: Proof era -> [Pred era]
vstatePreds p = case whichCertState p of
  CertStateShelleyToBabbage -> []
  CertStateConwayToConway -> vstateGenPreds p ++ vstateCheckPreds p

vstateGenPreds :: forall era. ConwayEraCertState era => Proof era -> [Pred era]
vstateGenPreds p = case whichPParams p of
  PParamsConwayToConway ->
    [ MetaSize (SzRng 5 15) currentDRepStateSize
    , Sized currentDRepStateSize currentDRepState
    , Sized (Range 5 7) (Dom committeeState)
    , Subset (Dom currentDRepState) voteUniv
    , Subset (Dom committeeState) voteCredUniv
    , Sized (Range 0 5) numDormantEpochs
    , ForEach
        currentDRepStateSize
        drepStateList
        (Pat DRepStateR [Arg expire, Arg anchor, Arg deposit])
        [ Random (fieldToTerm anchor)
        , GenFrom
            (fieldToTerm expire)
            (Constr "+200To500" (\(EpochNo n) -> EpochNo <$> choose (n + 200, n + 500)) ^$ currentEpoch)
        , drepDeposit p :=: (fieldToTerm deposit)
        ]
    , drepStateList :=: (Elems currentDRepState)
    , SumsTo (Left (Coin 1)) totalDRepDeposit EQL [ProjMap CoinR drepDepositL currentDRepState]
    ]
  _ ->
    [ Sized (Range 0 0) currentDRepState
    , Sized (Range 0 0) committeeState
    , Lit EpochR (EpochNo 0) :=: numDormantEpochs
    , Random currentDRepState
    ]
  where
    drepStateList = Var $ pV p "drepStateList" (ListR DRepStateR) No
    deposit = Field @era "deposit" CoinR DRepStateR drepDepositL
    anchor = Field @era "anchor" (MaybeR AnchorR) DRepStateR (drepAnchorL . strictMaybeToMaybeL)
    expire = Field @era "expire" EpochR DRepStateR drepExpiryL
    totalDRepDeposit = Var $ pV p "totalDRepDeposit" CoinR No
    currentDRepStateSize = Var $ pV p "currentDRepStateSize" SizeR No

vstateCheckPreds :: Proof era -> [Pred era]
vstateCheckPreds _p = []

vstateStage ::
  Reflect era =>
  Proof era ->
  Subst era ->
  Gen (Subst era)
vstateStage proof = toolChainSub proof standardOrderInfo (vstatePreds proof)

demoV :: ReplMode -> IO ()
demoV mode = do
  let proof = Conway
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

pstatePreds :: EraCertState era => Proof era -> [Pred era]
pstatePreds p = pstateGenPreds p ++ pstateCheckPreds p

pstateGenPreds :: EraCertState era => Proof era -> [Pred era]
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
  ]
  where
    e = var "e" EpochR
    epochs = var "epochs" (ListR EpochR)

pstateCheckPreds :: EraCertState era => Proof era -> [Pred era]
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
  let proof = Babbage
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
gdKeyHashField :: Era era => Field era GenDelegPair (KeyHash 'Witness)
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
gdKeyHash :: Era era => Term era (KeyHash 'Witness)
gdKeyHash = fieldToTerm gdKeyHashField

gdkeyL :: Lens' GenDelegPair (KeyHash 'Witness)
gdkeyL =
  lens
    (\(GenDelegPair x _) -> asWitness x)
    (\(GenDelegPair _ y) x -> GenDelegPair (coerceKeyRole x) y)

-- ============================================================================

certStatePreds :: EraCertState era => Proof era -> [Pred era]
certStatePreds p = certStateGenPreds p ++ certStateCheckPreds p

certStateGenPreds :: EraCertState era => Proof era -> [Pred era]
certStateGenPreds p =
  [ MetaSize (SzExact (fromIntegral (quorumConstant + 2))) genDelegSize
  , --  , GenFrom quorum (constTarget (pure (fromIntegral quorumConstant)))

    -- These really belong in the AccountState in the EpochState, But they are the only pieces of the EpochState we use.
    -- and are necessary to compute the instantaneous rewards, which are in the DState.
    GenFrom treasury (constTarget (Coin <$> choose (1000, 4000)))
  , GenFrom reserves (constTarget (Coin <$> choose (4000, 5000)))
  , GenFrom deltaTreasury (constTarget (DeltaCoin <$> choose (-200, 400)))
  , Negate deltaReserves :=: deltaTreasury -- Means deltaReserves ranges from (-400,200)
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
  , Sized (AtMost 8) delegations
  , Dom delegations :⊆: Dom rewards
  , Dom delegations :⊆: Dom instantStakeTerm
  , Rng delegations :⊆: Dom regPools
  , if protocolVersion p >= protocolVersion Conway
      then Sized (ExactSize 0) ptrs
      else Dom rewards :=: Rng ptrs
  , Dom drepDelegation :⊆: credsUniv
  , Rng drepDelegation :⊆: drepUniv
  , -- Preds to compute genDelegs
    -- First, a set of GenDelegPairs, where no keyHash is repeated.
    Sized genDelegSize gdKeyHashSet
  , ProjS gdkeyL WitHashR gdKeyHashSet `Subset` Dom keymapUniv
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
  , mirAvailTreasury
      :<-: ( Constr "computeAvailTreasury" (availableAfterMIR TreasuryMIR)
               :$ Mask accountStateT
               :$ Mask instantaneousRewardsT
           )
  , mirAvailReserves
      :<-: ( Constr "computeAvailReserves" (availableAfterMIR ReservesMIR)
               :$ Mask accountStateT
               :$ Mask instantaneousRewardsT
           )
  , Dom drepDelegation :⊆: credsUniv
  , Rng drepDelegation :⊆: drepUniv
  ]
  where
    rewardSize = var "rewardSize" SizeR
    rewardRange = var "rewardRange" (ListR CoinR)
    genDelegSize = var "genDelegSize" SizeR
    gdKeyHashSet = var "gdKeyHashSet" (SetR GenDelegPairR)
    gdKeyHashList = var "gdKeyHashList" (ListR GenDelegPairR)

certStateCheckPreds :: EraCertState era => Proof era -> [Pred era]
certStateCheckPreds p =
  [ NotMember (Lit CoinR (Coin 0)) (Rng stakeDeposits)
  , Dom rewards :=: Dom stakeDeposits
  , Dom delegations :⊆: Dom rewards
  , if protocolVersion p >= protocolVersion Conway
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
  let proof = Babbage
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

demoC :: ReplMode -> IO ()
demoC mode = do
  let proof = Conway
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
  certState <- monadTyped . runTarget env $ certStateT
  when (mode == Interactive) $ putStrLn (show (pcCertState (unCertStateF certState)))
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
