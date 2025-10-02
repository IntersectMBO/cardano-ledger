{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Imp (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import qualified Test.Cardano.Ledger.Allegra.Imp.UtxowSpec as UtxowSpec
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

spec :: forall era. (ShelleyEraImp era, EraSpecificSpec era) => Spec
spec = do
  ShelleyImp.spec @era
  describe "AllegraImpSpec" . withEachEraVersion @era $
    UtxowSpec.spec

instance EraSpecificSpec AllegraEra
