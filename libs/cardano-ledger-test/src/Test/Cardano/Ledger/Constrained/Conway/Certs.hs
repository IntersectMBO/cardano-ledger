-- | Specs necessary to generate, environment, state, and signal
-- for the CERTS rule
module Test.Cardano.Ledger.Constrained.Conway.Certs where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained
import Data.Sequence (Seq)

certsEnvSpec ::
  Specification fn (CertsEnv (ConwayEra StandardCrypto))
certsEnvSpec = TrueSpec

txCertsSpec ::
  Specification fn (Seq (ConwayTxCert (ConwayEra StandardCrypto)))
txCertsSpec = TrueSpec
