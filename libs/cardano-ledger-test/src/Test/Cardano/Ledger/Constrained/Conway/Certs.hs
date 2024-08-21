{-# LANGUAGE FlexibleContexts #-}

-- | Specs necessary to generate, environment, state, and signal
-- for the CERTS rule
module Test.Cardano.Ledger.Constrained.Conway.Certs where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Conway.TxCert
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained
import Data.Sequence (Seq)
import Test.Cardano.Ledger.Constrained.Conway.Instances
import Test.Cardano.Ledger.Constrained.Conway.PParams (pparamsSpec)

certsEnvSpec ::
  IsConwayUniv fn =>
  Specification fn (CertsEnv (ConwayEra StandardCrypto))
certsEnvSpec = constrained $ \env ->
  match env $ \_ pp _ _ _ _ ->
    satisfies pp pparamsSpec

txCertsSpec ::
  Specification fn (Seq (ConwayTxCert (ConwayEra StandardCrypto)))
txCertsSpec = TrueSpec
