module Test.Cardano.Ledger.Alonzo.Scripts
  ( alwaysSucceeds,
    alwaysFails,
    saltFunction,
  )
where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (Script (..))
import Data.ByteString.Short (ShortByteString)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Examples as Plutus
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
    saltFunction,
  )

alwaysSucceeds :: Language -> Natural -> Script era
alwaysSucceeds lang n = PlutusScript lang (Plutus.alwaysSucceedingNAryFunction n)

alwaysFails :: Language -> Natural -> Script era
alwaysFails lang n = PlutusScript lang (Plutus.alwaysFailingNAryFunction n)

saltFunction :: Language -> Integer -> ShortByteString -> Script era
saltFunction lang n ps = PlutusScript lang (Plutus.saltFunction n ps)
