{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test module where we check that the block validation implementation
-- matches the formal specification. To this end, the strategy is:
--
-- 0. generate traces of abstract blocks, which conform to the formal semantics
--    of the blockchain layer
--
-- 1. elaborate these abstract blocks into concrete blocks
--
-- 2. feed the generated sequence of concrete blocks to the block validation
--    function, and check that it passes the validation.
--
module Test.Cardano.Chain.Block.Validation.Spec
  ( tests
  , passConcreteValidation
  )
where

import Cardano.Prelude hiding (trace, State)

import Control.Lens ((^.))
import Hedgehog
  ( MonadTest
  , Property
  , checkParallel
  , discover
  , evalEither
  , forAll
  , property
  , withTests
  )

import Cardano.Chain.Block
  ( ChainValidationError
  , ChainValidationState
  , initialChainValidationState
  , updateBlock
  )
import Cardano.Crypto.ProtocolMagic (ProtocolMagic)
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import Control.State.Transition.Generator (trace)
import Control.State.Transition (State)
import Control.State.Transition.Trace
  (TraceOrder(OldestFirst), Trace, preStatesAndSignals, traceEnv)

import Test.Cardano.Chain.Elaboration.Block (elaborateBS, abEnvToCfg)
import Test.Cardano.Crypto.Gen (genProtocolMagic)

tests :: IO Bool
tests = checkParallel $$discover

-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
prop_generatedChainsAreValidated :: Property
prop_generatedChainsAreValidated =
  -- TODO: we might want to make this configurable, so that we run a smaller
  -- number of tests when developing, and use a higher number when on CI (or in
  -- nightly builds?).
  withTests 300 $ property $ do
    pm <- forAll genProtocolMagic
    tr <- forAll trace
    passConcreteValidation pm tr

passConcreteValidation :: MonadTest m => ProtocolMagic -> Trace CHAIN -> m ()
passConcreteValidation pm tr = void $ evalEither res
 where
  res = foldM elaborateAndUpdate initSt $ preStatesAndSignals OldestFirst tr

  elaborateAndUpdate
    :: ChainValidationState
    -> (State CHAIN, Abstract.Block)
    -> Either ChainValidationError ChainValidationState
  elaborateAndUpdate cst (ast, ab) = updateBlock
    config
    cst
    (elaborateBS config aenv ast cst ab)
    where aenv = tr ^. traceEnv

  initSt =
    either (panic . show) identity $ initialChainValidationState config

  config = abEnvToCfg pm (tr ^. traceEnv)
