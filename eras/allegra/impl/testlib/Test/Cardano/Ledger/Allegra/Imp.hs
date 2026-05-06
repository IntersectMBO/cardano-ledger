{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Imp (spec) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp as ShelleyImp

spec ::
  ( ShelleyEraImp era
  , EraSpecificSpec era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  ShelleyImp.spec era

instance EraSpecificSpec AllegraEra where
  eraSpecificSpec = ShelleyImp.shelleyEraSpecificSpec
