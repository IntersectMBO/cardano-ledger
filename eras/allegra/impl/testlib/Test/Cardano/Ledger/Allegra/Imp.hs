{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Imp (spec) where

import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Test.Cardano.Ledger.Allegra.ImpTest
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Shelley.Imp as Shelley

spec ::
  ( ShelleyEraImp era
  , Shelley.Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Shelley.spec era
  describe "AllegraEra Onwards" $ pure ()
