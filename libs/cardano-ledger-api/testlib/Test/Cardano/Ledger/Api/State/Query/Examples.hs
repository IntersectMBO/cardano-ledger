{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Api.State.Query.Examples (
  queryAccountsDepositsExamples,
  queryChainAccountStateExamples,
  queryConstitutionExamples,
  queryConstitutionHashExamples,
  queryCurrentEpochNoExamples,
  queryDRepDelegateesExamples,
  queryDRepDelegationsExamples,
  queryDRepStakeDistrExamples,
  queryDRepStateExamples,
  queryPoolParametersExamples,
  queryRegisteredDRepStakeDistrExamples,
  querySPOStakeDistrExamples,
  queryStakePoolDefaultVoteExamples,
  queryStakePoolDelegsAndRewardsExamples,
  queryStakePoolRelaysExamples,
) where

import Cardano.Base.IP (toIPv4, toIPv6)
import Cardano.Ledger.Api.Governance (Constitution (..))
import Cardano.Ledger.Api.State.Query (DefaultVote (..))
import Cardano.Ledger.BaseTypes (AnchorData, EpochNo (..), Port (..), StrictMaybe (..), textToDns)
import Cardano.Ledger.Coin (Coin (..), CompactForm (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Hashes (SafeHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.State (ChainAccountState (..), StakePoolParams (..), StakePoolRelay (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Conway.Examples (exampleAnchor)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (exampleStakePoolParams, mkKeyHash, mkScriptHash)

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
  [ ChainAccountState (Coin 0) (Coin 0)
  , ChainAccountState (Coin 1_500_000_000_000_000) (Coin 8_000_000_000_000_000)
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
