module Test.Cardano.Ledger.Api.State.Query.Examples (
  queryConstitutionExamples,
  queryConstitutionHashExamples,
  queryCurrentEpochNoExamples,
) where

import Cardano.Ledger.Api.Governance (Constitution (..))
import Cardano.Ledger.BaseTypes (AnchorData, EpochNo (..), StrictMaybe (..))
import Cardano.Ledger.Hashes (SafeHash)
import Test.Cardano.Ledger.Conway.Examples (exampleAnchor)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import Test.Cardano.Ledger.Shelley.Examples (mkScriptHash)

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
