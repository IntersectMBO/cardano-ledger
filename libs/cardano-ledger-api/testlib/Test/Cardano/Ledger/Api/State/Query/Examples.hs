module Test.Cardano.Ledger.Api.State.Query.Examples (
  queryConstitutionExamples,
  queryCurrentEpochNoExamples,
) where

import Cardano.Ledger.Api.Governance (Constitution (..))
import Cardano.Ledger.BaseTypes (EpochNo (..), StrictMaybe (..))
import Test.Cardano.Ledger.Conway.Examples (exampleAnchor)
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
