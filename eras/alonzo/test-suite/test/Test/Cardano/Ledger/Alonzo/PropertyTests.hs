{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- Embed instances for (AlonzoEra TestCrypto)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Alonzo.PropertyTests where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY, AlonzoLEDGER)
import Control.State.Transition
import Data.Proxy
import Test.Cardano.Ledger.Alonzo.EraMapping ()
import Test.Cardano.Ledger.Alonzo.Trace ()
import Test.Cardano.Ledger.EraBuffet (TestCrypto)
import qualified Test.Cardano.Ledger.Shelley.PropertyTests as Shelley
import Test.Cardano.Ledger.Shelley.Rules.Chain (
  CHAIN,
  ChainEvent (..),
  TestChainPredicateFailure (..),
 )
import Test.Cardano.Ledger.Shelley.Rules.IncrementalStake (incrStakeComparisonProp)
import Test.QuickCheck (
  withMaxSuccess,
 )
import Test.Tasty
import qualified Test.Tasty.QuickCheck as TQC

type A = AlonzoEra TestCrypto

instance Embed (AlonzoBBODY A) (CHAIN A) where
  wrapFailed = BbodyFailure
  wrapEvent = BbodyEvent

-- | The same property tests run in all the other Eras, specialized to the Alonzo Era.
alonzoPropertyTests :: TestTree
alonzoPropertyTests =
  testGroup
    "Alonzo property tests"
    [ Shelley.propertyTests @A @(AlonzoLEDGER A)
    , TQC.testProperty
        "Incremental stake distribution at epoch boundaries agrees"
        (incrStakeComparisonProp (Proxy :: Proxy A))
    ]

-- | A select subset of all the property tests
fastPropertyTests :: TestTree
fastPropertyTests =
  testGroup
    "Fast Alonzo Property Tests"
    [ TQC.testProperty
        "total amount of Ada is preserved (Chain)"
        (withMaxSuccess 50 (Shelley.adaPreservationProps @A @(AlonzoLEDGER A)))
    ]
