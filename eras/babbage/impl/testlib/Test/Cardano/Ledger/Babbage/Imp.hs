{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Ledger.Babbage.Imp (spec) where

import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Babbage.Imp.BabbageFeatures as BabbageFeatures
import Test.Cardano.Ledger.Babbage.ImpTest
import Cardano.Ledger.Babbage.Core (InjectRuleFailure, Value)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError)
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext(..))

spec :: forall era.
  ( BabbageEraImp era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  , Arbitrary (Value era)
  ) => Spec
spec = describe "BabbageImpSpec" $ withImpState @era $ do
  BabbageFeatures.spec @era

