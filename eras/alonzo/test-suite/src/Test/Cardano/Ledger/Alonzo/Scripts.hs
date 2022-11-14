module Test.Cardano.Ledger.Alonzo.Scripts
  ( alwaysSucceeds,
    alwaysFails,
    saltFunction,
  )
where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Data.ByteString.Short (ShortByteString)
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.Test.Examples as Plutus
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
    saltFunction,
  )

alwaysSucceeds :: Language -> Natural -> AlonzoScript era
alwaysSucceeds lang n = PlutusScript lang (Plutus.alwaysSucceedingNAryFunction n)

alwaysFails :: Language -> Natural -> AlonzoScript era
alwaysFails lang n = PlutusScript lang (Plutus.alwaysFailingNAryFunction n)

saltFunction :: Language -> Integer -> ShortByteString -> AlonzoScript era
saltFunction lang n ps = PlutusScript lang (Plutus.saltFunction n ps)
