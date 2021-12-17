module Test.Cardano.Ledger.Babbage.Scripts
  ( alwaysSucceeds,
    alwaysFails,
  )
where

import Cardano.Ledger.Babbage.Language (Language (..))
import Cardano.Ledger.Babbage.Scripts (Script (..))
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Examples as Plutus
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
  )

alwaysSucceeds :: Language -> Natural -> Script era
alwaysSucceeds lang n = PlutusScript lang (Plutus.alwaysSucceedingNAryFunction n)

alwaysFails :: Language -> Natural -> Script era
alwaysFails lang n = PlutusScript lang (Plutus.alwaysFailingNAryFunction n)
