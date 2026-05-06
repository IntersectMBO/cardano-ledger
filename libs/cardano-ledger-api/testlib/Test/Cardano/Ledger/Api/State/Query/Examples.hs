{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Api.State.Query.Examples (
  queryAccountsDepositsExamples,
  queryChainAccountStateExamples,
  queryCommitteeMembersStateExamples,
  queryConstitutionExamples,
  queryConstitutionHashExamples,
  queryCurrentEpochNoExamples,
  queryCurrentPParamsExamples,
  queryDRepDelegateesExamples,
  queryDRepDelegationsExamples,
  queryDRepStakeDistrExamples,
  queryDRepStateExamples,
  queryFuturePParamsExamples,
  queryGovStateExamples,
  queryPoolParametersExamples,
  queryPoolStateExamples,
  queryProposalsExamples,
  queryRatifyStateExamples,
  queryRegisteredDRepStakeDistrExamples,
  querySPOStakeDistrExamples,
  querySetSnapshotStakePoolDistrExamples,
  queryStakePoolDefaultVoteExamples,
  queryStakePoolDelegsAndRewardsExamples,
  queryStakePoolRelaysExamples,
  queryStakeSnapshotsExamples,
) where

import Cardano.Base.IP (toIPv4, toIPv6)
import Cardano.Ledger.Api.Governance (
  Committee (..),
  Constitution (..),
  ConwayGovState (..),
  DRepPulsingState (..),
  EnactState (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  RatifyState (..),
  Vote (..),
 )
import Cardano.Ledger.Api.PParams (
  PParams,
  ppEMaxL,
  ppKeyDepositL,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxTxSizeL,
  ppMinPoolCostL,
  ppNOptL,
  ppPoolDepositL,
 )
import Cardano.Ledger.Api.State.Query (
  CommitteeMemberState (..),
  CommitteeMembersState (..),
  DefaultVote (..),
  HotCredAuthStatus (..),
  MemberStatus (..),
  NextEpochChange (..),
  QueryPoolStateResult (..),
  StakeSnapshot (..),
  StakeSnapshots (..),
 )
import Cardano.Ledger.BaseTypes (
  AnchorData,
  EpochInterval (..),
  EpochNo (..),
  Port (..),
  StrictMaybe (..),
  UnitInterval,
  textToDns,
 )
import Cardano.Ledger.Coin (Coin (..), CompactForm (..), knownNonZeroCoin)
import Cardano.Ledger.Core (EraPParams)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (SafeHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.State (
  ChainAccountState (..),
  FuturePParams (..),
  IndividualPoolStake (..),
  PoolDistr (..),
  StakePoolParams (..),
  StakePoolRelay (..),
 )
import Cardano.Ledger.TxIn (TxId (..))
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Conway.Examples (
  exampleAnchor,
  exampleProposalProcedure,
  exampleProposalProcedureHardForkInitiation,
  exampleProposalProcedureNewConstitution,
  exampleProposalProcedureNoConfidence,
  exampleProposalProcedureParameterChange,
  exampleProposalProcedureTreasuryWithdrawals,
  exampleProposalProcedureUpdateCommittee,
 )
import Test.Cardano.Ledger.Core.Rational (unsafeBoundRational)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (
  examplePoolDistr,
  exampleStakePoolParams,
  exampleVrfVerKeyHash,
  mkKeyHash,
  mkScriptHash,
 )

queryConstitutionExamples :: [Constitution era]
queryConstitutionExamples =
  [ Constitution
      { constitutionAnchor = exampleAnchor
      , constitutionGuardrailsScriptHash = SJust (mkScriptHash 1)
      }
  , Constitution
      { constitutionAnchor = exampleAnchor
      , constitutionGuardrailsScriptHash = SNothing
      }
  ]

queryCurrentEpochNoExamples :: [EpochNo]
queryCurrentEpochNoExamples =
  [ EpochNo 0
  , EpochNo 500
  , EpochNo maxBound
  ]

populatedPParams :: EraPParams era => PParams era
populatedPParams =
  def
    & ppMaxBBSizeL .~ 90112
    & ppMaxTxSizeL .~ 16384
    & ppMaxBHSizeL .~ 1100
    & ppKeyDepositL .~ Coin 2_000_000
    & ppPoolDepositL .~ Coin 500_000_000
    & ppMinPoolCostL .~ Coin 340_000_000
    & ppEMaxL .~ EpochInterval 18
    & ppNOptL .~ 500

queryCurrentPParamsExamples :: EraPParams era => [PParams era]
queryCurrentPParamsExamples = [def, populatedPParams]

queryConstitutionHashExamples :: [SafeHash AnchorData]
queryConstitutionHashExamples =
  [ mkDummySafeHash 0
  , mkDummySafeHash 1
  , mkDummySafeHash 42
  ]

queryAccountsDepositsExamples :: [Map (Credential Staking) Coin]
queryAccountsDepositsExamples =
  [ Map.empty
  , Map.fromList
      [ (KeyHashObj (mkKeyHash 1), Coin 2_000_000)
      , (ScriptHashObj (mkScriptHash 2), Coin 0)
      ]
  ]

queryChainAccountStateExamples :: [ChainAccountState]
queryChainAccountStateExamples =
  [ ChainAccountState
      { casTreasury = Coin 0
      , casReserves = Coin 0
      }
  , ChainAccountState
      { casTreasury = Coin 1_500_000_000_000_000
      , casReserves = Coin 8_000_000_000_000_000
      }
  ]

queryCommitteeMembersStateExamples :: [CommitteeMembersState]
queryCommitteeMembersStateExamples =
  [ CommitteeMembersState
      { csCommittee = Map.empty
      , csThreshold = Nothing
      , csEpochNo = EpochNo 0
      }
  , CommitteeMembersState
      { csCommittee =
          Map.fromList
            [
              ( KeyHashObj (mkKeyHash 1)
              , CommitteeMemberState
                  { cmsHotCredAuthStatus = MemberAuthorized (KeyHashObj (mkKeyHash 11))
                  , cmsStatus = Active
                  , cmsExpiration = Just (EpochNo 200)
                  , cmsNextEpochChange = NoChangeExpected
                  }
              )
            ,
              ( KeyHashObj (mkKeyHash 2)
              , CommitteeMemberState
                  { cmsHotCredAuthStatus = MemberNotAuthorized
                  , cmsStatus = Expired
                  , cmsExpiration = Just (EpochNo 100)
                  , cmsNextEpochChange = ToBeRemoved
                  }
              )
            ,
              ( ScriptHashObj (mkScriptHash 3)
              , CommitteeMemberState
                  { cmsHotCredAuthStatus = MemberResigned (Just exampleAnchor)
                  , cmsStatus = Unrecognized
                  , cmsExpiration = Nothing
                  , cmsNextEpochChange = ToBeEnacted
                  }
              )
            ,
              ( KeyHashObj (mkKeyHash 4)
              , CommitteeMemberState
                  { cmsHotCredAuthStatus = MemberResigned Nothing
                  , cmsStatus = Active
                  , cmsExpiration = Just (EpochNo 300)
                  , cmsNextEpochChange = TermAdjusted (EpochNo 350)
                  }
              )
            ,
              ( KeyHashObj (mkKeyHash 5)
              , CommitteeMemberState
                  { cmsHotCredAuthStatus = MemberAuthorized (ScriptHashObj (mkScriptHash 50))
                  , cmsStatus = Active
                  , cmsExpiration = Just (EpochNo 250)
                  , cmsNextEpochChange = ToBeExpired
                  }
              )
            ]
      , csThreshold = Just (unsafeBoundRational (2 % 3) :: UnitInterval)
      , csEpochNo = EpochNo 150
      }
  ]

querySPOStakeDistrExamples :: [Map (KeyHash StakePool) Coin]
querySPOStakeDistrExamples =
  [ Map.empty
  , Map.fromList
      [ (mkKeyHash 1, Coin 1_000_000_000)
      , (mkKeyHash 2, Coin 0)
      , (mkKeyHash 3, Coin 50)
      ]
  ]

querySetSnapshotStakePoolDistrExamples :: [PoolDistr]
querySetSnapshotStakePoolDistrExamples =
  [ def
  , examplePoolDistr
  , PoolDistr
      { unPoolDistr =
          Map.fromList
            [
              ( mkKeyHash 1
              , IndividualPoolStake
                  { individualPoolStake = 1 % 4
                  , individualTotalPoolStake = CompactCoin 1_000_000_000
                  , individualPoolStakeVrf = exampleVrfVerKeyHash
                  }
              )
            ,
              ( mkKeyHash 2
              , IndividualPoolStake
                  { individualPoolStake = 3 % 8
                  , individualTotalPoolStake = CompactCoin 5_000_000_000
                  , individualPoolStakeVrf = exampleVrfVerKeyHash
                  }
              )
            ]
      , pdTotalActiveStake = knownNonZeroCoin @6_000_000_000
      }
  ]

queryStakePoolDefaultVoteExamples :: [DefaultVote]
queryStakePoolDefaultVoteExamples =
  [ DefaultNo
  , DefaultAbstain
  , DefaultNoConfidence
  ]

queryStakePoolDelegsAndRewardsExamples ::
  [(Map (Credential Staking) (KeyHash StakePool), Map (Credential Staking) Coin)]
queryStakePoolDelegsAndRewardsExamples =
  [ (Map.empty, Map.empty)
  ,
    ( Map.fromList
        [ (KeyHashObj (mkKeyHash 1), mkKeyHash 10)
        , (ScriptHashObj (mkScriptHash 2), mkKeyHash 20)
        ]
    , Map.fromList
        [ (KeyHashObj (mkKeyHash 1), Coin 1_000_000)
        , (ScriptHashObj (mkScriptHash 2), Coin 0)
        ]
    )
  ]

queryStakePoolRelaysExamples :: [Map (KeyHash StakePool) (Rational, StrictSeq StakePoolRelay)]
queryStakePoolRelaysExamples =
  [ Map.empty
  , Map.fromList
      [
        ( mkKeyHash 1
        ,
          ( 1 % 4
          , StrictSeq.fromList
              [ SingleHostAddr
                  (SJust (Port 3001))
                  (SJust (toIPv4 [192, 168, 1, 1]))
                  (SJust (toIPv6 [0x2001, 0xdb8, 0, 0, 0, 0, 0, 1]))
              , SingleHostName
                  (SJust (Port 3001))
                  (fromJust (textToDns 64 "pool-1.relay.example"))
              ]
          )
        )
      ,
        ( mkKeyHash 2
        ,
          ( 3 % 8
          , StrictSeq.fromList
              [ SingleHostAddr SNothing (SJust (toIPv4 [10, 0, 0, 5])) SNothing
              , MultiHostName (fromJust (textToDns 64 "_relay._tcp.pool-2.example"))
              ]
          )
        )
      ]
  ]

queryDRepDelegateesExamples :: [Map (Credential Staking) DRep]
queryDRepDelegateesExamples =
  [ Map.empty
  , Map.fromList
      [ (KeyHashObj (mkKeyHash 1), DRepKeyHash (mkKeyHash 10))
      , (ScriptHashObj (mkScriptHash 2), DRepScriptHash (mkScriptHash 20))
      , (KeyHashObj (mkKeyHash 3), DRepAlwaysAbstain)
      , (KeyHashObj (mkKeyHash 4), DRepAlwaysNoConfidence)
      ]
  ]

queryDRepDelegationsExamples :: [Map DRep (Set (Credential Staking))]
queryDRepDelegationsExamples =
  [ Map.empty
  , Map.fromList
      [
        ( DRepKeyHash (mkKeyHash 1)
        , Set.fromList [KeyHashObj (mkKeyHash 10), ScriptHashObj (mkScriptHash 11)]
        )
      , (DRepScriptHash (mkScriptHash 2), Set.singleton (KeyHashObj (mkKeyHash 20)))
      , (DRepAlwaysAbstain, Set.singleton (KeyHashObj (mkKeyHash 30)))
      , (DRepAlwaysNoConfidence, Set.empty)
      ]
  ]

queryDRepStakeDistrExamples :: [Map DRep Coin]
queryDRepStakeDistrExamples =
  [ Map.empty
  , Map.fromList
      [ (DRepKeyHash (mkKeyHash 1), Coin 1_000_000_000)
      , (DRepScriptHash (mkScriptHash 2), Coin 0)
      , (DRepAlwaysAbstain, Coin 50)
      , (DRepAlwaysNoConfidence, Coin 100)
      ]
  ]

queryDRepStateExamples :: [Map (Credential DRepRole) DRepState]
queryDRepStateExamples =
  [ Map.empty
  , Map.fromList
      [
        ( KeyHashObj (mkKeyHash 1)
        , DRepState
            { drepExpiry = EpochNo 100
            , drepAnchor = SJust exampleAnchor
            , drepDeposit = CompactCoin 500_000_000
            , drepDelegs = mempty
            }
        )
      ,
        ( ScriptHashObj (mkScriptHash 2)
        , DRepState
            { drepExpiry = EpochNo 0
            , drepAnchor = SNothing
            , drepDeposit = CompactCoin 0
            , drepDelegs = mempty
            }
        )
      ]
  ]

queryRatifyStateExamples ::
  EraPParams era =>
  [RatifyState era]
queryRatifyStateExamples =
  [ def
  , RatifyState
      { rsEnactState =
          EnactState
            { ensCommittee =
                SJust $
                  Committee
                    { committeeMembers =
                        Map.singleton (KeyHashObj (mkKeyHash 1)) (EpochNo 200)
                    , committeeThreshold = unsafeBoundRational (1 % 2) :: UnitInterval
                    }
            , ensConstitution =
                Constitution
                  { constitutionAnchor = exampleAnchor
                  , constitutionGuardrailsScriptHash = SJust (mkScriptHash 1)
                  }
            , ensCurPParams = def
            , ensPrevPParams = def
            , ensTreasury = Coin 1_000_000_000
            , ensWithdrawals =
                Map.fromList
                  [ (KeyHashObj (mkKeyHash 1), Coin 500_000_000)
                  , (ScriptHashObj (mkScriptHash 2), Coin 250_000_000)
                  ]
            , ensPrevGovActionIds = def
            }
      , rsEnacted =
          Seq.fromList
            [ GovActionState
                { gasId = mkGid 1
                , gasCommitteeVotes = Map.empty
                , gasDRepVotes = Map.empty
                , gasStakePoolVotes = Map.empty
                , gasProposalProcedure = exampleProposalProcedure
                , gasProposedIn = EpochNo 100
                , gasExpiresAfter = EpochNo 130
                }
            ]
      , rsExpired = Set.fromList [mkGid 99]
      , rsDelayed = True
      }
  ]
  where
    mkGid n =
      GovActionId
        { gaidTxId = TxId (mkDummySafeHash n)
        , gaidGovActionIx = GovActionIx 0
        }

queryRegisteredDRepStakeDistrExamples :: [Map (Credential DRepRole) Coin]
queryRegisteredDRepStakeDistrExamples =
  [ Map.empty
  , Map.fromList
      [ (KeyHashObj (mkKeyHash 1), Coin 1_000_000_000)
      , (ScriptHashObj (mkScriptHash 2), Coin 0)
      ]
  ]

queryPoolParametersExamples :: [Map (KeyHash StakePool) StakePoolParams]
queryPoolParametersExamples =
  [ Map.empty
  , Map.fromList
      [ (sppId exampleStakePoolParams, exampleStakePoolParams)
      ,
        ( mkKeyHash 99
        , exampleStakePoolParams
            { sppId = mkKeyHash 99
            , sppPledge = Coin 100_000_000
            , sppCost = Coin 340_000_000
            , sppRelays =
                StrictSeq.fromList
                  [ SingleHostAddr (SJust (Port 3001)) (SJust (toIPv4 [10, 0, 0, 1])) SNothing
                  , MultiHostName (fromJust (textToDns 64 "_relay._tcp.pool-99.example"))
                  ]
            }
        )
      ,
        ( mkKeyHash 100
        , exampleStakePoolParams
            { sppId = mkKeyHash 100
            , sppPledge = Coin 0
            , sppCost = Coin 170_000_000
            , sppMetadata = SNothing
            }
        )
      ]
  ]

queryPoolStateExamples :: [QueryPoolStateResult]
queryPoolStateExamples =
  [ QueryPoolStateResult
      { qpsrStakePoolParams = Map.empty
      , qpsrFutureStakePoolParams = Map.empty
      , qpsrRetiring = Map.empty
      , qpsrDeposits = Map.empty
      }
  , QueryPoolStateResult
      { qpsrStakePoolParams =
          Map.fromList
            [ (sppId exampleStakePoolParams, exampleStakePoolParams)
            , (mkKeyHash 99, exampleStakePoolParams {sppId = mkKeyHash 99})
            ]
      , qpsrFutureStakePoolParams =
          Map.singleton
            (mkKeyHash 100)
            (exampleStakePoolParams {sppId = mkKeyHash 100})
      , qpsrRetiring = Map.singleton (mkKeyHash 99) (EpochNo 250)
      , qpsrDeposits =
          Map.fromList
            [ (mkKeyHash 1, Coin 500_000_000)
            , (mkKeyHash 99, Coin 500_000_000)
            ]
      }
  ]

queryFuturePParamsExamples :: EraPParams era => [Maybe (PParams era)]
queryFuturePParamsExamples = [Nothing, Just def, Just populatedPParams]

queryGovStateExamples ::
  EraPParams era =>
  [ConwayGovState era]
queryGovStateExamples =
  [ def
  , ConwayGovState
      { cgsProposals = def
      , cgsCommittee =
          SJust $
            Committee
              { committeeMembers =
                  Map.fromList
                    [ (KeyHashObj (mkKeyHash 1), EpochNo 200)
                    , (KeyHashObj (mkKeyHash 2), EpochNo 300)
                    , (ScriptHashObj (mkScriptHash 3), EpochNo 250)
                    ]
              , committeeThreshold = unsafeBoundRational (2 % 3) :: UnitInterval
              }
      , cgsConstitution =
          Constitution
            { constitutionAnchor = exampleAnchor
            , constitutionGuardrailsScriptHash = SJust (mkScriptHash 1)
            }
      , cgsCurPParams = def
      , cgsPrevPParams = def
      , cgsFuturePParams = DefinitePParamsUpdate def
      , cgsDRepPulsingState = DRComplete def def
      }
  ]

queryProposalsExamples :: EraPParams era => [Seq (GovActionState era)]
queryProposalsExamples =
  [ Seq.empty
  , Seq.fromList
      [ GovActionState
          { gasId = mkGid 1
          , gasCommitteeVotes = Map.empty
          , gasDRepVotes = Map.empty
          , gasStakePoolVotes = Map.empty
          , gasProposalProcedure = exampleProposalProcedure
          , gasProposedIn = EpochNo 100
          , gasExpiresAfter = EpochNo 130
          }
      , GovActionState
          { gasId = mkGid 2
          , gasCommitteeVotes = Map.singleton (KeyHashObj (mkKeyHash 1)) VoteYes
          , gasDRepVotes = Map.singleton (KeyHashObj (mkKeyHash 2)) VoteNo
          , gasStakePoolVotes = Map.singleton (mkKeyHash 3) Abstain
          , gasProposalProcedure = exampleProposalProcedureHardForkInitiation
          , gasProposedIn = EpochNo 50
          , gasExpiresAfter = EpochNo 80
          }
      , GovActionState
          { gasId = mkGid 3
          , gasCommitteeVotes = Map.empty
          , gasDRepVotes = Map.empty
          , gasStakePoolVotes = Map.empty
          , gasProposalProcedure = exampleProposalProcedureTreasuryWithdrawals
          , gasProposedIn = EpochNo 60
          , gasExpiresAfter = EpochNo 90
          }
      , GovActionState
          { gasId = mkGid 4
          , gasCommitteeVotes = Map.empty
          , gasDRepVotes =
              Map.fromList
                [ (KeyHashObj (mkKeyHash 4), Abstain)
                , (ScriptHashObj (mkScriptHash 5), VoteYes)
                ]
          , gasStakePoolVotes = Map.empty
          , gasProposalProcedure = exampleProposalProcedureNoConfidence
          , gasProposedIn = EpochNo 70
          , gasExpiresAfter = EpochNo 100
          }
      , GovActionState
          { gasId = mkGid 5
          , gasCommitteeVotes = Map.empty
          , gasDRepVotes = Map.empty
          , gasStakePoolVotes = Map.empty
          , gasProposalProcedure = exampleProposalProcedureUpdateCommittee
          , gasProposedIn = EpochNo 80
          , gasExpiresAfter = EpochNo 110
          }
      , GovActionState
          { gasId = mkGid 6
          , gasCommitteeVotes = Map.empty
          , gasDRepVotes = Map.empty
          , gasStakePoolVotes = Map.empty
          , gasProposalProcedure = exampleProposalProcedureNewConstitution
          , gasProposedIn = EpochNo 90
          , gasExpiresAfter = EpochNo 120
          }
      , GovActionState
          { gasId = mkGid 7
          , gasCommitteeVotes = Map.singleton (KeyHashObj (mkKeyHash 11)) VoteYes
          , gasDRepVotes = Map.singleton (KeyHashObj (mkKeyHash 22)) VoteYes
          , gasStakePoolVotes = Map.singleton (mkKeyHash 33) VoteYes
          , gasProposalProcedure = exampleProposalProcedureParameterChange
          , gasProposedIn = EpochNo 100
          , gasExpiresAfter = EpochNo 130
          }
      ]
  ]
  where
    mkGid n =
      GovActionId
        { gaidTxId = TxId (mkDummySafeHash n)
        , gaidGovActionIx = GovActionIx 0
        }

queryStakeSnapshotsExamples :: [StakeSnapshots]
queryStakeSnapshotsExamples =
  [ StakeSnapshots
      { ssStakeSnapshots = Map.empty
      , ssMarkTotal = knownNonZeroCoin @1
      , ssSetTotal = knownNonZeroCoin @1
      , ssGoTotal = knownNonZeroCoin @1
      }
  , StakeSnapshots
      { ssStakeSnapshots =
          Map.fromList
            [
              ( mkKeyHash 1
              , StakeSnapshot
                  { ssMarkPool = Coin 1_000_000_000
                  , ssSetPool = Coin 900_000_000
                  , ssGoPool = Coin 800_000_000
                  }
              )
            ,
              ( mkKeyHash 2
              , StakeSnapshot
                  { ssMarkPool = Coin 0
                  , ssSetPool = Coin 500_000_000
                  , ssGoPool = Coin 250_000_000
                  }
              )
            ]
      , ssMarkTotal = knownNonZeroCoin @5_000_000_000
      , ssSetTotal = knownNonZeroCoin @4_500_000_000
      , ssGoTotal = knownNonZeroCoin @4_000_000_000
      }
  ]
