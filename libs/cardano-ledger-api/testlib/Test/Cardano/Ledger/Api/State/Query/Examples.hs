module Test.Cardano.Ledger.Api.State.Query.Examples (
  queryConstitutionExamples,
) where

import Cardano.Ledger.Api.Governance (Constitution (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
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
