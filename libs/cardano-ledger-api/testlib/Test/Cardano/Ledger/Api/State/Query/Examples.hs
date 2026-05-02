{-# LANGUAGE NumericUnderscores #-}

module Test.Cardano.Ledger.Api.State.Query.Examples (
  queryAccountsDepositsExamples,
  queryConstitutionExamples,
  queryConstitutionHashExamples,
  queryCurrentEpochNoExamples,
  queryDRepStakeDistrExamples,
  queryRegisteredDRepStakeDistrExamples,
  querySPOStakeDistrExamples,
) where

import Cardano.Ledger.Api.Governance (Constitution (..))
import Cardano.Ledger.BaseTypes (AnchorData, EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..))
import Cardano.Ledger.Hashes (SafeHash)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Conway.Examples (exampleAnchor)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (mkKeyHash, mkScriptHash)

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

querySPOStakeDistrExamples :: [Map (KeyHash StakePool) Coin]
querySPOStakeDistrExamples =
  [ Map.empty
  , Map.fromList
      [ (mkKeyHash 1, Coin 1_000_000_000)
      , (mkKeyHash 2, Coin 0)
      , (mkKeyHash 3, Coin 50)
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

queryRegisteredDRepStakeDistrExamples :: [Map (Credential DRepRole) Coin]
queryRegisteredDRepStakeDistrExamples =
  [ Map.empty
  , Map.fromList
      [ (KeyHashObj (mkKeyHash 1), Coin 1_000_000_000)
      , (ScriptHashObj (mkScriptHash 2), Coin 0)
      ]
  ]
