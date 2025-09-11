{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.NewEpoch () where

import Cardano.Ledger.Conway (ConwayEra)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (ExecSpecRule (..), SpecTRC (..), unComputationResult_)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Conway.ImpTest ()

instance ExecSpecRule "NEWEPOCH" ConwayEra where
  runAgdaRule (SpecTRC env st sig) = fmap fst . unComputationResult_ $ Agda.newEpochStep env st sig
