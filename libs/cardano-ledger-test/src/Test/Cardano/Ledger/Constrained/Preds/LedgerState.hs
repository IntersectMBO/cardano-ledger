{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Constrained.Preds.LedgerState where

import Cardano.Ledger.Alonzo.PParams (ppuMaxValSizeL)
import Cardano.Ledger.Babbage.PParams (ppuCoinsPerUTxOByteL)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionId (..),
  GovActionPurpose (..),
  GovActionState (..),
  ProposalProcedure (..),
  Proposals,
  gasAction,
  gasActionL,
  gasDeposit,
  gasIdL,
  pPropsL,
  proposalsActions,
 )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppuDRepDepositL,
  ppuMinFeeRefScriptCostPerByteL,
 )
import Cardano.Ledger.Core (
  Era (..),
  PParamsUpdate,
  ppuMaxTxSizeL,
  ppuMinFeeAL,
  ppuMinFeeBL,
 )
import Cardano.Ledger.DRep (drepDepositL)
import Control.Monad (when)
import Data.Default.Class (Default (def))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.OMap.Strict as OMap
import Data.Ratio ((%))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Ast
import Test.Cardano.Ledger.Constrained.Classes (OrdCond (..), genPParamsUpdate)
import Test.Cardano.Ledger.Constrained.Combinators (itemFromSet)
import Test.Cardano.Ledger.Constrained.Env
import Test.Cardano.Ledger.Constrained.Monad (monadTyped)
import Test.Cardano.Ledger.Constrained.Preds.CertState (dstateStage, pstateStage, vstateStage)
import Test.Cardano.Ledger.Constrained.Preds.PParams (pParamsStage)
import Test.Cardano.Ledger.Constrained.Preds.Repl (ReplMode (..), modeRepl)
import Test.Cardano.Ledger.Constrained.Preds.UTxO (utxoStage)
import Test.Cardano.Ledger.Constrained.Preds.Universes (UnivSize (..), universeStage)
import Test.Cardano.Ledger.Constrained.Rewrite (standardOrderInfo)
import Test.Cardano.Ledger.Constrained.Size (Size (..))
import Test.Cardano.Ledger.Constrained.Solver (toolChainSub)
import Test.Cardano.Ledger.Constrained.TypeRep
import Test.Cardano.Ledger.Constrained.Utils (testIO)
import Test.Cardano.Ledger.Constrained.Vars
import Test.Cardano.Ledger.Generic.PrettyCore (pcLedgerState)
import Test.Cardano.Ledger.Generic.Proof
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain)
import Type.Reflection (typeRep)

-- =========================================

prevGovActionIdsGenPreds :: Reflect era => Proof era -> [Pred era]
prevGovActionIdsGenPreds _ =
  [ Random prevGovActionIds
  , Random hardForkChildren
  , Random committeeChildren
  , Random constitutionChildren
  ]

prevGovActionIdsCheckPreds :: Proof era -> [Pred era]
prevGovActionIdsCheckPreds _ = []

enactStateGenPreds :: Reflect era => Proof era -> [Pred era]
enactStateGenPreds p =
  [ Random committeeVar
  , Random constitution
  , prevPParams p :<-: (Constr "id" id ^$ pparams p)
  , currPParams p :<-: (Constr "id" id ^$ pparams p)
  , Random enactTreasury
  , Random enactWithdrawals
  , -- PrevGovActionsIds constraints
    Subset (Dom prevDRepState) voteUniv
  , Subset (Dom partialDRepDistr) drepUniv
  ]
    ++ prevGovActionIdsGenPreds p

enactStateCheckPreds :: Proof era -> [Pred era]
enactStateCheckPreds _ = []

ledgerStatePreds :: forall era. Reflect era => UnivSize -> Proof era -> [Pred era]
ledgerStatePreds _usize p =
  [ Subset (Dom enactWithdrawals) credsUniv
  , Random enactTreasury
  , Random constitution
  , Random committeeVar
  , Random ppUpdateChildren
  , Random hardForkChildren
  , Random committeeChildren
  , Random constitutionChildren
  , proposalDeposits
      :<-: ( Constr "sumActionStateDeposits" (foldMap gasDeposit . proposalsActions)
              :$ (Simple $ currProposals p)
           )
  , -- TODO, introduce ProjList so we can write: SumsTo (Right (Coin 1)) proposalDeposits  EQL [ProjList CoinR gasDepositL currProposals]
    SumsTo
      (Right (Coin 1))
      deposits
      EQL
      [ SumMap stakeDeposits
      , SumMap poolDeposits
      , One proposalDeposits
      , ProjMap CoinR drepDepositL currentDRepState
      ]
  , -- Some things we might want in the future.
    -- , SumsTo (Right (Coin 1)) utxoCoin EQL [ProjMap CoinR outputCoinL (utxo p)]
    -- , SumsTo (Right (Coin 1)) totalAda EQL [One utxoCoin, One treasury, One reserves, One fees, One deposits, SumMap rewards]
    Random fees
  , ledgerState :<-: (ledgerStateT p)
  , Sized (Range 1 10) donation
  , prevPParams p :<-: (Constr "id" id ^$ (pparams p))
  , currPParams p :<-: (Constr "id" id ^$ (pparams p))
  , -- We need the poolDistr to generate a valid Pulser
    Dom regPools :=: Dom poolDistr
  , SumsTo (Left (1 % 1000)) (Lit RationalR 1) EQL [ProjMap RationalR individualPoolStakeL poolDistr]
  ]
    ++ ( case whichGovState p of
          GovStateConwayToConway ->
            [ Random randomProposals
            , currProposals p :<-: (Constr "reasonable" reasonable ^$ randomProposals)
            , Random (futurePParams p)
            ]
              ++ prevPulsingPreds p -- Constraints to generate a valid Pulser
          GovStateShelleyToBabbage ->
            [ Sized (Range 0 1) (pparamProposals p)
            , Sized (Range 0 1) (futurePParamProposals p)
            , Random (futurePParams p)
            ]
       )
  where
    randomProposals = Var (pV p "randomProposals" (ProposalsR p) No)

ledgerStateStage ::
  Reflect era =>
  UnivSize ->
  Proof era ->
  Subst era ->
  Gen (Subst era)
ledgerStateStage usize proof subst0 = do
  let preds = ledgerStatePreds usize proof
  subst <- toolChainSub proof standardOrderInfo preds subst0
  (_env, status) <- pure (error "not used in ledgerStateStage", Nothing) -- monadTyped $ checkForSoundness preds subst
  case status of
    Nothing -> pure subst
    Just msg -> error msg

demo :: Reflect era => Proof era -> ReplMode -> IO ()
demo proof mode = do
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= utxoStage def proof
          >>= vstateStage proof
          >>= pstateStage proof
          >>= dstateStage proof
          >>= ledgerStateStage def proof
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  lstate <- monadTyped $ runTarget env (ledgerStateT proof)
  let env2 = getTarget lstate (ledgerStateT proof) env
  when (mode == Interactive) $ putStrLn (show (pcLedgerState proof lstate))
  modeRepl mode proof env2 ""

demoTest :: TestTree
demoTest = testIO "Testing LedgerState Stage" (demo Conway CI)

main :: IO ()
main = defaultMain $ testIO "Testing LedgerState Stage" (demo Conway Interactive)

-- =============================================
-- Contraint based approach to generating Proposals

-- | Generate the (parent,child) pairs in a Tree.
--   Be sure the list of 'a' are unique, because each node should appear in the Tree only once.
--   The first one will be the root of the tree
--   and have parent Nothing.
genTree :: Ord a => [a] -> Gen [(Maybe a, a)]
genTree [] = pure []
genTree (root : others) = genTreeHelp [root] [(Nothing, root)] (Set.fromList others)
  where
    genTreeHelp :: Ord a => [a] -> [(Maybe a, a)] -> Set a -> Gen [(Maybe a, a)]
    genTreeHelp _ edges nodes | Set.null nodes = pure (reverse edges)
    genTreeHelp roots edges nodes = do
      (x, more) <- itemFromSet [] nodes
      n <- choose (0, length roots - 1)
      genTreeHelp (x : roots) ((Just (roots !! n), x) : edges) more

{-
One might test genTree, something like this
go :: IO [(Maybe Int, Int)]
go = generate (genTree [1, 2, 3, 4, 5, 6, 7 :: Int])
-}

-- | Tie together GovActionState and GovAction using the (parent,child) links
--   that describe the shape of the Tree
useTriples ::
  [(Maybe (GovActionId (EraCrypto era)), GovActionId (EraCrypto era))] ->
  [GovAction era] ->
  [GovActionState era] ->
  [GovActionState era]
useTriples pairs as gs = zipWith3 help pairs as gs
  where
    help (parent, idx) a g =
      g
        & gasIdL .~ idx
        & gasActionL .~ setActionId a parent

-- | [Pred era] that generate a valid (Map GovActionId GovActionState)
govStatePreds :: forall era. (ConwayEraPParams era, Reflect era) => Proof era -> [Pred era]
govStatePreds p =
  [ MetaSize (SzRng 2 5) numActions
  , Sized numActions gaids
  , Subset gaids govActionIdUniv
  , GenFrom pairs (Constr "genTree" (genTree . Set.toList) ^$ gaids)
  , ListWhere
      numActions
      preGovstates
      govActionStateTarget
      [ Member (Left idV) govActionIdUniv
      , Subset (Dom committeeVotesV) hotCommitteeCredsUniv
      , Sized (Range 0 3) (Dom committeeVotesV)
      , Subset (Dom drepVotesV) voteUniv
      , Sized (Range 0 3) (Dom drepVotesV)
      , Subset (Dom stakePoolVotesV) poolHashUniv
      , Sized (Range 0 3) (Dom stakePoolVotesV)
      , proposalDeposit p :=: depositV
      , Random returnAddrV
      , Random anchorV
      , Oneof
          actionV
          [ (1, noConfidenceT, [Random gaPrevId])
          ,
            ( 1
            , updateCommitteeT
            ,
              [ Random gaPrevId
              , Sized (Range 0 3) gaRemMember
              , Subset gaRemMember coldCommitteeCredsUniv
              , Sized (Range 0 3) (Dom gaAddMember)
              , Subset (Dom gaAddMember) coldCommitteeCredsUniv
              , Random gaThreshold
              ]
            )
          ]
      , currentEpoch :=: proposedInV
      , Random expiresAfterV
      , Sized (Range 0 3) childrenV
      , Subset childrenV govActionIdUniv
      ]
  , ListWhere
      numActions
      govActions
      (Invert "GovAction" (typeRep @(GovAction era)) id :$ Partial govAction Just)
      [ Oneof
          govAction
          [ (1, noConfidenceT, [Random gaPrevId])
          ,
            ( 1
            , updateCommitteeT
            ,
              [ Random gaPrevId
              , Sized (Range 0 3) gaRemMember
              , Subset gaRemMember coldCommitteeCredsUniv
              , Sized (Range 0 3) (Dom gaAddMember)
              , Subset (Dom gaAddMember) coldCommitteeCredsUniv
              , Random gaThreshold
              ]
            )
          ]
      ]
  , govActionStates :<-: (Constr "useTriples" useTriples ^$ pairs ^$ govActions ^$ preGovstates)
  , govActionMap :<-: (Constr "toProposalMap" toProposalMap ^$ govActionStates)
  ]
  where
    gaids = Var (pV p "gaids" (SetR GovActionIdR) No)
    pairs = Var (pV p "pairs" (ListR (PairR (MaybeR GovActionIdR) GovActionIdR)) No)
    numActions = Var (pV p "numActions" SizeR No)
    preGovstates = Var (V "preGovstates" (ListR GovActionStateR) No)
    govActionStates = Var (pV p "govActionStates" (ListR (GovActionStateR)) No)
    govAction = Var (pV p "govAction" GovActionR No)
    govActions = Var (pV p "govActions" (ListR GovActionR) No)
    govActionMap = Var (pV p "govActionMap" (MapR GovActionIdR GovActionStateR) No)

toProposalMap ::
  forall era. [GovActionState era] -> Map.Map (GovActionId (EraCrypto era)) (GovActionState era)
toProposalMap xs = Map.fromList (map pairup xs)
  where
    pairup gas = (gasId gas, gas)

demoGov :: (ConwayEraPParams era, Reflect era) => Proof era -> ReplMode -> IO ()
demoGov proof mode = do
  env <-
    generate
      ( pure emptySubst
          >>= pParamsStage proof
          >>= universeStage def proof
          >>= toolChainSub proof standardOrderInfo (govStatePreds proof)
          >>= (\subst -> monadTyped $ substToEnv subst emptyEnv)
      )
  modeRepl mode proof env ""

mainGov :: IO ()
mainGov = demoGov Conway Interactive

setActionId :: GovAction era -> Maybe (GovActionId (EraCrypto era)) -> GovAction era
setActionId (ParameterChange _ pp p) x = ParameterChange (liftId x) pp p
setActionId (HardForkInitiation _ y) x = HardForkInitiation (liftId x) y
setActionId (UpdateCommittee _ w y z) x = UpdateCommittee (liftId x) w y z
setActionId (NewConstitution _ y) x = NewConstitution (liftId x) y
setActionId InfoAction _ = InfoAction
setActionId (NoConfidence _) x = NoConfidence (liftId x)
setActionId x@(TreasuryWithdrawals _ _) _ = x

actionIdL :: Lens' (GovAction era) (Maybe (GovActionId (EraCrypto era)))
actionIdL = lens getter setter
  where
    getter (ParameterChange x _ _) = dropId x
    getter (HardForkInitiation x _) = dropId x
    getter (UpdateCommittee x _ _ _) = dropId x
    getter (NewConstitution x _) = dropId x
    getter (NoConfidence x) = dropId x
    getter (TreasuryWithdrawals _ _) = Nothing
    getter InfoAction = Nothing
    setter ga mid = setActionId ga mid

children :: GovActionId c -> [(Maybe (GovActionId c), GovActionId c)] -> Set (GovActionId c)
children x ys = List.foldl' accum Set.empty ys
  where
    accum ans (Just y, z) | x == y = Set.insert z ans
    accum ans _ = ans

genGovActionStates ::
  forall era.
  Era era =>
  Proof era ->
  Set (GovActionId (EraCrypto era)) ->
  Gen (Map.Map (GovActionId (EraCrypto era)) (GovActionState era))
genGovActionStates proof gaids = do
  pairs <- genTree (Set.toList gaids)
  let genGovState (parent, idx) = do
        state <-
          GovActionState
            <$> pure idx
            <*> pure Map.empty
            <*> pure Map.empty
            <*> pure Map.empty
            <*> ( ProposalProcedure
                    <$> genRep @era CoinR
                    <*> arbitrary
                    <*> genGovAction proof CommitteePurpose parent
                    <*> arbitrary
                )
            <*> arbitrary
            <*> arbitrary
        pure (state & gasActionL .~ setActionId (gasAction state) parent)
  states <- mapM genGovState pairs
  pure (Map.fromList (map (\x -> (gasId x, x)) states))

genGovAction ::
  forall era.
  Era era =>
  Proof era ->
  GovActionPurpose ->
  Maybe (GovActionId (EraCrypto era)) ->
  Gen (GovAction era)
genGovAction proof purpose gaid = case purpose of
  PParamUpdatePurpose -> ParameterChange (liftId gaid) <$> (unPParamsUpdate <$> genPParamsUpdate proof) <*> arbitrary
  HardForkPurpose -> HardForkInitiation (liftId gaid) <$> arbitrary
  CommitteePurpose ->
    frequency
      [ (1, pure (NoConfidence (liftId gaid)))
      ,
        ( 1
        , UpdateCommittee (liftId gaid)
            <$> genSizedRep @era 2 (SetR CommColdCredR)
            <*> genSizedRep @era 3 (MapR CommColdCredR EpochR)
            <*> arbitrary
        )
      ]
  ConstitutionPurpose -> NewConstitution (liftId gaid) <$> arbitrary

-- ===========================================================================================
-- Make sure that any PParamsUpdate in a Proposal does not change certain crucial parameters
-- We currently generate well formed, but unreasonable ParameterChange GovActions.
-- depending on what kind of Tx you generate, you might want to ensure some PParam fields
-- take on reasonable values for your Tx generator.

mapOMap :: OMap.HasOKey k v => (v -> v) -> OMap.OMap k v -> OMap.OMap k v
mapOMap f x = foldr accum OMap.empty x
  where
    accum y ys = f y OMap.<| ys

updateProposals :: (GovAction era -> GovAction era) -> Proposals era -> Proposals era
updateProposals f x = x & pPropsL %~ (mapOMap (\y -> y & gasActionL %~ f))

updateGovAction :: (PParamsUpdate era -> PParamsUpdate era) -> GovAction era -> GovAction era
updateGovAction g (ParameterChange x y z) = ParameterChange x (g y) z
updateGovAction _ x = x

-- | What is reasonable for a Tx generated in Conway by 'drepCertTx'
-- Well parameters that affect the Fee, the Tx Size, TxOuts, and DRep Deposits
reasonable :: ConwayEraPParams era => Proposals era -> Proposals era
reasonable =
  updateProposals
    ( updateGovAction
        ( \x ->
            x
              & ppuMaxTxSizeL .~ SNothing
              & ppuMinFeeAL .~ SNothing
              & ppuMinFeeBL .~ SNothing
              & ppuMaxValSizeL .~ SNothing
              & ppuCoinsPerUTxOByteL .~ SNothing
              & ppuMinFeeRefScriptCostPerByteL .~ SNothing
              & ppuDRepDepositL .~ SNothing
        )
    )
