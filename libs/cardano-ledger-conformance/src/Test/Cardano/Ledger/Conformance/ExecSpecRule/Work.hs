{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (
  Committee (..),
  EnactState (..),
  GovAction (..),
  GovActionState (..),
  GovProcedures (..),
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
import Test.Cardano.Ledger.Generic.PrettyCore (PrettyA (..))
import Test.QuickCheck hiding (forAll)

import Cardano.Ledger.BaseTypes (EpochInterval (..), EpochNo (..))
import Cardano.Ledger.CertState (CommitteeState (..))
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  ppCommitteeMaxTermLengthL,
 )
import Data.Foldable (toList)
import Lens.Micro
import Test.Cardano.Ledger.Constrained.Conway.Instances (ConwayFn)

import Data.Maybe.Strict

import Cardano.Ledger.CertState (CommitteeAuthorization (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
 )
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Data.Sequence (Seq (..))
import Data.Sequence.Strict (StrictSeq)
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)

-- ==========================================================================

hotCred :: RatifyEnv Conway -> Set (Credential 'HotCommitteeRole StandardCrypto)
hotCred x = Map.foldl' accum Set.empty (csCommitteeCreds (reCommitteeState x))
  where
    accum ans (CommitteeHotCredential c) = Set.insert c ans
    accum ans (CommitteeMemberResigned _) = ans

ratifyEnvSpec :: Specification ConwayFn (RatifyEnv Conway)
ratifyEnvSpec = constrained $ \ [var|renv|] ->
  match renv $ \ [var|stakedistr|] [var|pooldistr|] [var|drepdistr|] [var|drepstate|] [var|curEpoch|] [var|comState|] ->
    [ assert $ curEpoch ==. lit (10)
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

ratifyStateSpec :: PParams Conway -> RatifyEnv Conway -> Specification ConwayFn (RatifyState Conway)
ratifyStateSpec pp env = constrained $ \ [var|rstate|] ->
  match rstate $ \ [var|enactstate|] [var|enacted|] [var|sexpired|] [var|delayed|] ->
    match enactstate $ \_ _ [var|currPP|] [var|prevPP|] [var|treasury|] _ _ ->
      [ assert $ delayed ==. lit False
      , assert $ currPP ==. lit pp
      , satisfies enacted (enactedSpec pp env treasury)
      ]

ratifySignalSpec ::
  RatifyEnv Conway ->
  RatifyState Conway ->
  Specification ConwayFn (RatifySignal Conway)
ratifySignalSpec
  env@(RatifyEnv stakedistr pooldist drepdistr drepstate curEpoch comstate)
  (RatifyState EnactState {ensCurPParams = pparams, ensTreasury = tcoin} enacted expired delayed) =
    constrained $ \ [var|signal|] ->
      match signal $ \ [var|sigseq|] ->
        match sigseq $ \ [var|siglist|] ->
          [ assert $ sizeOf_ siglist <=. lit (fromIntegral (length enacted))
          , forAll siglist $ \ [var|govactstate|] -> assert $ govactstate `elem_` lit (toList enacted)
          ]

enactedSpec ::
  PParams Conway ->
  RatifyEnv Conway ->
  Term ConwayFn Coin ->
  Specification ConwayFn (Seq (GovActionState Conway))
enactedSpec pp env treasuryCoin =
  constrained $ \seqgas ->
    match seqgas $ \listgas ->
      forAll listgas $ \ [var|govactstate|] ->
        match govactstate $ \_ [var|cvotes|] [var|dvotes|] [var|pvotes|] [var|proposalprocedure|] [var|proposed|] [var|expires|] ->
          [ genHint 4 listgas
          , satisfies cvotes (voteSpec (hotCred env)) -- clean this up
          , satisfies dvotes (voteSpec (Map.keysSet (reDRepState env)))
          , satisfies pvotes (voteSpec (Map.keysSet (unPoolDistr (reStakePoolDistr env))))
          , match proposalprocedure $ \_ _ [var|govaction|] _ ->
              [ (caseOn govaction)
                  (branch $ \_ _ _ -> True) -- ParameterChange
                  (branch $ \_ _ -> True) -- HardFork
                  (branch $ \ [var|coinmap|] _ -> sum_ (rng_ coinmap) ==. treasuryCoin) -- TreasuryWithdrawal
                  (branch $ \_ -> True) -- NoConfidence
                  ( branch $ \_ [var|remmembers|] [var|newmembers|] _ ->
                      -- UpdateCommittee
                      [ forAll newmembers $ \pair ->
                          match pair $ \_cred [var|epoch|] ->
                            assertExplain
                              ["newmembers have valid maxTermLength"]
                              (epoch <=. lit (addEpoch (reCurrentEpoch env) (pp ^. ppCommitteeMaxTermLengthL)))
                      ]
                  )
                  (branch $ \_ _ -> True) -- NewConstitution
                  (branch $ \_ -> True) -- InfoAction
              ]
          ]

voteSpec :: (HasSpec ConwayFn a, Ord a) => Set a -> Specification ConwayFn (Map a Vote)
voteSpec goodAs = constrained $ \mp ->
  [ assert $ sizeOf_ mp <=. 4
  , assert $ mp /=. lit (Map.empty)
  , forAll mp $ \pair ->
      match pair $ \a v ->
        [ (caseOn v)
            (branchW 1 $ \_ -> True) -- VoteNo
            (branchW 10 $ \_ -> True) -- VoteYes
            (branchW 1 $ \_ -> True) -- Abstain
        , assert $ member_ a (lit goodAs)
        ]
  ]

main :: IO ()
main = do
  pp <- generate $ genFromSpec (pparamsSpec @ConwayFn)
  env <- generate $ genFromSpec ratifyEnvSpec
  state <- generate $ genFromSpec (ratifyStateSpec pp env)
  -- printPlan (ratifySignalSpec env state)
  sig@(RatifySignal zs) <- generate $ genFromSpec (ratifySignalSpec env state)
  -- putStrLn (show (prettyA sig) ++"\n"++ show (length zs))
  -- putStrLn "\n\n\n=================================================================="
  -- putStrLn (show (prettyA env))
  putStrLn (show (prettyA sig))
  putStrLn ("Enacted length = " ++ show (length (rsEnacted state)))
  putStrLn ("Signal length = " ++ show (length zs))

addEpoch :: EpochNo -> EpochInterval -> EpochNo
addEpoch (EpochNo m) (EpochInterval n) = EpochNo (m + fromIntegral n)

coldCred :: RatifyState Conway -> Set (Credential 'ColdCommitteeRole StandardCrypto)
coldCred x = case (ensCommittee . rsEnactState) x of
  SNothing -> Set.empty
  SJust x -> Map.keysSet (committeeMembers x)
