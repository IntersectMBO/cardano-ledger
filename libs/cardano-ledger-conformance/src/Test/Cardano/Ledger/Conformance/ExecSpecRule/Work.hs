{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionId (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovProcedures (..),
  GovPurposeId (..),
  GovRelation (..),
  ProposalProcedure (..),
  Proposals,
  RatifyEnv (..),
  RatifySignal (..),
  RatifyState (..),
  Vote (..),
  VotingProcedures,
  ensPrevGovActionIdsL,
  gasAction,
  pRootsL,
  toPrevGovActionIds,
 )
import Constrained
import Constrained.Base
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..), ppSet, ppWord64)
import Test.QuickCheck hiding (forAll)

import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochNo (..))
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppCommitteeMaxTermLengthL,
 )
import Data.Foldable (toList)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances (ConwayFn, IsConwayUniv)

import Data.Maybe.Strict

import Cardano.Ledger.CertState (CommitteeAuthorization (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Compactible (fromCompact, toCompact)
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
 )
import Cardano.Ledger.Core (Era (..), PParams)
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Data.Maybe (fromJust)
import Data.Sequence (Seq (..))
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Word (Word64)
import Debug.Trace
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof hiding (reify)

-- ==========================================================================

hotCred :: RatifyEnv Conway -> Set (Credential 'HotCommitteeRole StandardCrypto)
hotCred x = Map.foldl' accum Set.empty (csCommitteeCreds (reCommitteeState x))
  where
    accum ans (CommitteeHotCredential c) = Set.insert c ans
    accum ans (CommitteeMemberResigned _) = ans

drepCred :: forall c x. Map (DRep c) x -> Set (Credential 'DRepRole c)
drepCred m = Set.foldl' accum Set.empty (Map.keysSet m)
  where
    accum ans (DRepCredential cred) = Set.insert cred ans
    accum ans _ = ans

{-
data RatifyEnv era
  = RatifyEnv {reStakeDistr :: !(Map
                                   (Credential 'Staking (EraCrypto era))
                                   (Cardano.Ledger.Compactible.CompactForm Coin)),
               reStakePoolDistr :: !(PoolDistr (EraCrypto era)),
               reDRepDistr :: !(Map
                                  (DRep (EraCrypto era))
                                  (Cardano.Ledger.Compactible.CompactForm Coin)),
               reDRepState :: !(Map
                                  (Credential 'DRepRole (EraCrypto era))
                                  (DRepState (EraCrypto era))),
               reCurrentEpoch :: ! {-# UNPACK #-}(Cardano.Slotting.Slot.N:EpochNo[0])EpochNo,
               reCommitteeState :: !(CommitteeState era)}

 [ satisfies cvotes (voteSpec (hotCred env)) -- clean this up
          , satisfies dvotes (voteSpec (Map.keysSet (reDRepState env)))
          , satisfies pvotes (voteSpec (Map.keysSet (unPoolDistr (reStakePoolDistr env))))

dvotes :: -> Map (Credential 'DRepRole (EraCrypto era)) Vote

DRepCredential :: Credential 'DRepRole c -> DRep c

-}

cc :: Integer -> Term ConwayFn (CompactForm Coin)
cc x = lit $ fromJust (toCompact (fromIntegral x))

instance IsConwayUniv fn => OrdLike fn (CompactForm Coin)

ratifyEnvSpec :: Specification ConwayFn (RatifyEnv Conway)
ratifyEnvSpec = constrained $ \ [var|renv|] ->
  match renv $ \ [var|stakedistr|] [var|pooldistr|] [var|drepdistr|] [var|drepstate|] [var|curEpoch|] [var|comState|] ->
    [ assert $ curEpoch ==. lit (10)
    , dependsOn drepstate drepdistr
    , satisfies drepdistr (drepDistrSpec 10)
    , {-
      , assert $ sizeOf_ drepdistr ==. 10
      , forAll drepdistr $ \ [var|drepPair|] ->
              match drepPair $ \ cred coin ->
                [caseOn cred
                  (branchW 6 $ \ _ ->  [assert $ coin >=. lit(CompactCoin 15), assert $ coin <=. lit(CompactCoin 100)])
                  (branchW 6 $ \ _ ->  [assert $ coin >=. lit(CompactCoin 15), assert $ coin <=. lit(CompactCoin 100)])
                  (branchW 1 $ \ _ ->  [assert $ coin <=. lit(CompactCoin 3),  assert $ coin >. lit(CompactCoin 1)])
                  (branchW 1 $ \ _ ->  [assert $ coin <=. lit(CompactCoin 3),  assert $ coin >. lit(CompactCoin 1)])
                ]
      -}
      reify drepdistr drepCred (\creds -> dom_ drepstate ==. creds)
    , match comState $ \cmap ->
        [ assert $ sizeOf_ cmap ==. 15
        , forAll cmap $ \pair ->
            match pair $ \k cauth ->
              [ (caseOn cauth)
                  (branchW 4 $ \_ -> True)
                  (branchW 1 $ \_ -> True)
              ]
        ]
    ]

ratifyStateSpec ::
  PParams Conway ->
  RatifyEnv Conway ->
  GovRelation StrictMaybe Conway ->
  Specification ConwayFn (RatifyState Conway)
ratifyStateSpec pp env rel = constrained $ \ [var|rstate|] ->
  match rstate $ \ [var|enactstate|] [var|enacted|] [var|sexpired|] [var|delayed|] ->
    [ assert $ delayed ==. lit False
    , match enactstate $ \_ _ [var|currPP|] [var|prevPP|] [var|treasury|] _ [var|prev|] ->
        [ assert $ currPP ==. lit pp
        , assert $ prev ==. lit rel
        , assert $ treasury ==. lit (Coin 10000)
        , satisfies enacted (enactedSpec pp env rel treasury)
        ]
    ]

unLit :: HasSpec fn a => Term fn a -> a
unLit (Lit x) = x
unLit other = error ("UnLit on non literal: " ++ show other)

ratifySignalSpec ::
  PParams Conway ->
  RatifyEnv Conway ->
  GovRelation StrictMaybe Conway ->
  RatifyState Conway ->
  Specification ConwayFn (RatifySignal Conway)
ratifySignalSpec
  pp
  env@(RatifyEnv stakedistr pooldist drepdistr drepstate curEpoch comstate)
  rel
  ( RatifyState
      EnactState {ensCurPParams = pparams, ensTreasury = tcoin, ensPrevGovActionIds = prevGaid}
      enacted
      expired
      delayed
    ) =
    constrained $ \ [var|signal|] ->
      match signal $ \ [var|sigseq|] ->
        match sigseq $ \ [var|siglist|] ->
          [ genHint 5 siglist
          , forAll siglist $ \ [var|govactstate|] -> satisfies govactstate (govActionStateSpec pp env rel (lit (Coin 1000)))
          ]

-- | Choose a size of the signal list.
--   1) Shouldn't be 0 too often
--   2) Should never be larger that the length of the enacted state ('maxSize')
--   3) Should not be too long (5)
sigsize :: Integer -> Specification ConwayFn Integer
sigsize maxSize =
  chooseSpec
    (1, equalSpec 0)
    (4, constrained $ \x -> [x >. lit 0, x <=. lit maxSize, x <=. lit 5])

enactedSpec ::
  PParams Conway ->
  RatifyEnv Conway ->
  GovRelation StrictMaybe Conway ->
  Term ConwayFn Coin ->
  Specification ConwayFn (Seq (GovActionState Conway))
enactedSpec pp env rel treasuryCoin =
  constrained $ \seqgas ->
    match seqgas $ \listgas ->
      forAll listgas $ \ [var|govactstate|] -> satisfies govactstate (govActionStateSpec pp env rel treasuryCoin)

data PTag t where
  PParamTag :: PTag PParamUpdatePurpose
  HardForkTag :: PTag HardForkPurpose
  CommitteeTag :: PTag CommitteePurpose
  ConstitutionTag :: PTag ConstitutionPurpose

findParent ::
  forall era purpose.
  GovRelation StrictMaybe era ->
  PTag purpose ->
  StrictMaybe (GovPurposeId purpose era)
findParent govrel tag =
  case (tag, govrel) of
    (PParamTag, GovRelation x _ _ _) -> x
    (HardForkTag, GovRelation _ x _ _) -> x
    (CommitteeTag, GovRelation _ _ x _) -> x
    (ConstitutionTag, GovRelation _ _ _ x) -> x

govActionStateSpec ::
  PParams Conway ->
  RatifyEnv Conway ->
  GovRelation StrictMaybe Conway ->
  Term ConwayFn Coin ->
  Specification ConwayFn (GovActionState Conway)
govActionStateSpec pp env rel treasuryCoin =
  constrained $ \ [var|govactstate|] ->
    match govactstate $ \_ [var|cvotes|] [var|dvotes|] [var|pvotes|] [var|proposalprocedure|] [var|proposed|] [var|expires|] ->
      [ satisfies cvotes (voteSpec (hotCred env)) -- clean this up
      , satisfies dvotes (voteSpec (Map.keysSet (reDRepState env)))
      , satisfies pvotes (voteSpec (Map.keysSet (unPoolDistr (reStakePoolDistr env))))
      , match proposalprocedure $ \_ _ [var|govaction|] _ ->
          [ (caseOn govaction)
              -- ParameterChange
              (branchW 4 $ \prev _ _ -> prev ==. lit (findParent rel PParamTag))
              -- HardFork
              (branchW 1 $ \prev _ -> prev ==. lit (findParent rel HardForkTag))
              -- TreasuryWithdrawal
              (branchW 4 $ \ [var|coinmap|] _ -> sum_ (rng_ coinmap) <=. treasuryCoin)
              -- NoConfidence
              (branchW 1 $ \prev -> prev ==. lit (findParent rel CommitteeTag))
              -- UpdateCommittee
              ( branchW 1 $ \prev [var|remmembers|] [var|newmembers|] _ ->
                  [ assert $ prev ==. lit (findParent rel CommitteeTag)
                  , forAll newmembers $ \pair ->
                      match pair $ \_cred [var|epoch|] ->
                        assertExplain
                          ["newmembers have valid maxTermLength"]
                          (epoch <=. lit (addEpoch (reCurrentEpoch env) (pp ^. ppCommitteeMaxTermLengthL)))
                  ]
              )
              -- NewConstitution
              (branchW 1 $ \prev _ -> prev ==. lit (findParent rel ConstitutionTag))
              -- InforAction
              (branchW 4 $ \_ -> True) -- InfoAction
          ]
      ]

voteSpec :: (HasSpec ConwayFn a, Ord a) => Set a -> Specification ConwayFn (Map a Vote)
voteSpec goodAs = constrained $ \mp ->
  [ assert $ genHint 10 mp
  , assert $ mp /=. lit (Map.empty)
  , genHint 8 mp
  , forAll mp $ \pair ->
      match pair $ \a v ->
        [ (caseOn v)
            (branchW 1 $ \_ -> True) -- VoteNo
            (branchW 20 $ \_ -> True) -- VoteYes
            (branchW 1 $ \_ -> True) -- Abstain
        , assert $ member_ a (lit goodAs)
        ]
  ]

main :: IO ()
main = do
  pp <- generate $ genFromSpec (pparamsSpec @ConwayFn)
  env <- generate $ genFromSpec ratifyEnvSpec
  rel <- generate $ genFromSpec @ConwayFn TrueSpec
  state <- generate $ genFromSpec (ratifyStateSpec pp env rel)
  -- printPlan (ratifySignalSpec pp env rel state)
  sig@(RatifySignal zs) <- generate $ genFromSpec (ratifySignalSpec pp env rel state)
  -- putStrLn "\n\n\n=================================================================="
  -- putStrLn (show (prettyA env))
  putStrLn ("DRep Distr " ++ show (prettyA (Map.map fromCompact (reDRepDistr env))))
  putStrLn (show (prettyA sig))
  putStrLn ("Enacted length = " ++ show (length (rsEnacted state)))
  putStrLn ("Signal length = " ++ show (length zs))

-- putStrLn ("PrevGovActIds = "++show (prettyA (ensPrevGovActionIds (rsEnactState state))))
-- putStrLn ("Computed PrevGovActIds = "++show (prettyA rel))

addEpoch :: EpochNo -> EpochInterval -> EpochNo
addEpoch (EpochNo m) (EpochInterval n) = EpochNo (m + fromIntegral n)

coldCred :: RatifyState Conway -> Set (Credential 'ColdCommitteeRole StandardCrypto)
coldCred x = case (ensCommittee . rsEnactState) x of
  SNothing -> Set.empty
  SJust x -> Map.keysSet (committeeMembers x)

test = do
  pp <- genFromSpec (pparamsSpec @ConwayFn)
  env <- genFromSpec ratifyEnvSpec
  rel <- arbitrary
  state <- genFromSpec (ratifyStateSpec pp env rel)
  sig@(RatifySignal zs) <- genFromSpec (ratifySignalSpec pp env rel state)
  let !_ =
        if length zs > 0
          then
            trace
              ( "\n***********************\n DRep Distr, length = "
                  ++ show (length zs)
                  ++ "\n"
                  ++ show (prettyA (Map.map fromCompact (reDRepDistr env)))
                  ++ "\n"
                  ++ show (prettyA (reDRepState env))
              )
              True
          else True
  goSTS
    (RATIFY Conway)
    env
    state
    sig
    ( \x -> case x of
        Left ps -> pure $ classify True "FAIL" (property True)
        Right newstate ->
          pure $
            classify
              True
              ("SUCCEED" ++ " " ++ show (state == newstate) ++ " " ++ show (length zs))
              (property True)
    )

instance Arbitrary (RatifySignal Conway) where
  arbitrary = RatifySignal . fromList <$> arbitrary

test2 = do
  pp <- genFromSpec (pparamsSpec @ConwayFn)
  env <- genFromSpec ratifyEnvSpec
  rel <- arbitrary
  state <- genFromSpec (ratifyStateSpec pp env rel)
  sig@(RatifySignal zs) <- genFromSpec (ratifySignalSpec pp env rel state)
  pure $ property (not (rsDelayed state))

go :: (HasSpec ConwayFn x, PrettyA x) => Specification ConwayFn x -> IO ()
go x = do
  xx <- generate $ genFromSpec x
  putStrLn (show (prettyA xx))

{-

This is the test
(prevActionAsExpected gas ensPrevGovActionIds, "Previous Action")

-- | Check that the previous governance action id specified in the proposal
-- does match the last one of the same purpose that was enacted.
prevActionAsExpected :: GovActionState era -> GovRelation StrictMaybe era -> Bool
prevActionAsExpected gas prevGovActionIds =
  withGovActionParent gas True $ \govRelationL parent _ -> parent == prevGovActionIds ^. govRelationL

-- | Apply a function to a GovAction that can have a parent.
withGovActionParent ::
  GovActionState era ->
  -- | The result to be used for governance actions that can't have a parent
  a ->
  -- | Function that will be applied to a lens and a parent
  ( forall p.
    (forall f. Lens' (GovRelation f era) (f (GovPurposeId p era))) ->
    StrictMaybe (GovPurposeId p era) -> -- GovAction Parent
    GovPurposeId p era ->
    a
  ) ->
  a
withGovActionParent gas noParent f =
  case gas ^. gasActionL of
    ParameterChange parent _ _ -> f grPParamUpdateL parent (GovPurposeId (gas ^. gasIdL))
    HardForkInitiation parent _ -> f grHardForkL parent (GovPurposeId (gas ^. gasIdL))
    TreasuryWithdrawals _ _ -> noParent
    NoConfidence parent -> f grCommitteeL parent (GovPurposeId (gas ^. gasIdL))
    UpdateCommittee parent _ _ _ -> f grCommitteeL parent (GovPurposeId (gas ^. gasIdL))
    NewConstitution parent _ -> f grConstitutionL parent (GovPurposeId (gas ^. gasIdL))
    InfoAction -> noParent

 govAction = gasAction gas
 -- gasAction :: GovActionState era -> GovAction era gasAction = pProcGovAction . gasProposalProcedure

data GovActionState era
  = GovActionState {gasId :: !(cardano-ledger-conway-1.15.1.0:Cardano.Ledger.Conway.Governance.Procedures.GovActionId
                                 (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Core.Era.EraCrypto
                                    era)),
                    gasCommitteeVotes :: !(Map
                                             (Credential
                                                'HotCommitteeRole
                                                (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Core.Era.EraCrypto
                                                   era))
                                             Vote),
                    gasDRepVotes :: !(Map
                                        (Credential
                                           'DRepRole
                                           (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Core.Era.EraCrypto
                                              era))
                                        Vote),
                    gasStakePoolVotes :: !(Map
                                             (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Keys.Internal.KeyHash
                                                'StakePool
                                                (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Core.Era.EraCrypto
                                                   era))
                                             Vote),
                    gasProposalProcedure :: !(ProposalProcedure era),
                    gasProposedIn :: ! {-# UNPACK #-}(Cardano.Slotting.Slot.N:EpochNo[0])EpochNo,
                    gasExpiresAfter :: ! {-# UNPACK #-}(Cardano.Slotting.Slot.N:EpochNo[0])EpochNo}

data ProposalProcedure era
  = ProposalProcedure {pProcDeposit :: !Coin,
                       pProcReturnAddr :: !(Cardano.Ledger.Address.RewardAccount
                                              (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Core.Era.EraCrypto
                                                 era)),
                       pProcGovAction :: !(GovAction era),
                       pProcAnchor :: !(Cardano.Ledger.BaseTypes.Anchor
                                          (cardano-ledger-core-1.13.1.0:Cardano.Ledger.Core.Era.EraCrypto
                                             era))}

-}

mapRestrictedValues :: Integer -> Specification ConwayFn (Map (Either Int ()) Int)
mapRestrictedValues n = constrained $ \m ->
  [ assert $ sizeOf_ (dom_ m) ==. lit n
  , forAll' m $ \k v ->
      caseOn
        k
        (branch $ \_ -> 20 <=. v)
        (branch $ \_ -> True)
  ]

mapSetSmall :: Specification ConwayFn (Map (Set Int) Int)
mapSetSmall = constrained $ \x ->
  forAll (dom_ x) $ \d ->
    assert $ subset_ d $ lit (Set.fromList [3 .. 4])

-- | The goal is to generate the map, where the Coins for the domain keys
--  DRepAlwaysAbstain, DRepAlwaysNoConfidence are small (<= 3)
-- and DRepKeyHash DRepScriptHash are large (>= 15)
-- But this fails. Using exists here might be a mistake?
drepDistrSpec :: Integer -> Specification ConwayFn (Map (DRep StandardCrypto) (CompactForm Coin))
drepDistrSpec n = constrained $ \drepdistr ->
  [ assert $ sizeOf_ drepdistr ==. lit n
  , forAll drepdistr $ \ [var|drepPair|] ->
      match drepPair $ \cred coin ->
        [ dependsOn coin cred
        , caseOn
            cred
            -- DReKeyHash
            (branchW 1 $ \_ -> [assert $ coin >=. lit (CompactCoin 15)]) -- , assert $ coin <=. lit(CompactCoin 25)])
            -- DRepScriptHash
            (branchW 1 $ \_ -> [assert $ coin >=. lit (CompactCoin 15)]) -- , assert $ coin <=. lit(CompactCoin 25)])
            -- DRepAlwaysAbstain
            (branchW 1 $ \_ -> [assert $ coin <=. lit (CompactCoin 3), assert $ coin >. lit (CompactCoin 1)])
            -- DRepNoConfidence
            (branchW 1 $ \_ -> [assert $ coin <=. lit (CompactCoin 3), assert $ coin >. lit (CompactCoin 1)])
        ]
  ]

instance PrettyA (CompactForm Coin) where
  prettyA (CompactCoin n) = ppWord64 n

instance (PrettyA x, PrettyA y) => PrettyA (Either x y) where
  prettyA (Left x) = "(Left " <> prettyA x <> ")"
  prettyA (Right x) = "(Right " <> prettyA x <> ")"

instance PrettyA x => PrettyA (Set x) where
  prettyA xs = ppSet prettyA xs
