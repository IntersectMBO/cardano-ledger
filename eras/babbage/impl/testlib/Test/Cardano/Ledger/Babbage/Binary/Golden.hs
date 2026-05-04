{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Binary.Golden (
  spec,
  module Test.Cardano.Ledger.Alonzo.Binary.Golden,
) where

import Cardano.Ledger.Alonzo.Core (ShelleyEraTxCert)
import Test.Cardano.Ledger.Alonzo.Binary.Golden hiding (spec)
import qualified Test.Cardano.Ledger.Alonzo.Binary.Golden as AlonzoGolden
import Test.Cardano.Ledger.Babbage.Era (BabbageEraTest)
import Test.Cardano.Ledger.Common (Spec)

spec ::
  forall era.
  ( BabbageEraTest era
  , ShelleyEraTxCert era
  ) =>
  Spec
spec = do
  AlonzoGolden.spec @era
