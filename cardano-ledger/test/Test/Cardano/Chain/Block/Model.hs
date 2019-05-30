{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

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
module Test.Cardano.Chain.Block.Model
  ( tests
  , passConcreteValidation
  )
where

import Cardano.Prelude hiding (trace, State)
import Test.Cardano.Prelude

import Control.Lens ((^.))
import Hedgehog (MonadTest, PropertyT, collect, evalEither, forAll, property)

import Cardano.Chain.Block
  ( ChainValidationError
  , ChainValidationState(cvsUtxo)
  , initialChainValidationState
  , updateBlock
  )
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.UTxO as Concrete
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import Control.State.Transition.Generator (classifyTraceLength, trace)
import Control.State.Transition (State)
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(NewestFirst, OldestFirst)
  , preStatesAndSignals
  , traceEnv
  , traceSignals
  )
import qualified Ledger.UTxO as Abstract

import Test.Cardano.Chain.Elaboration.Block (elaborateBS, abEnvToCfg, rcDCert)
import Test.Cardano.Chain.UTxO.Model (elaborateInitialUTxO)
import Test.Options (TSGroup, TSProperty, withTestsTS)


tests :: TSGroup
tests = $$discoverPropArg


-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
ts_prop_generatedChainsAreValidated :: TSProperty
ts_prop_generatedChainsAreValidated =
  withTestsTS 100 $ property $ do
    tr <- forAll $ trace @CHAIN 100
    classifyTraceLength tr 100 50
    passConcreteValidation tr


passConcreteValidation :: MonadTest m => Trace CHAIN -> m ()
passConcreteValidation tr = void $ evalEither res
 where
  res =
    foldM (elaborateAndUpdate config) (initialState, txIdMap)
      $ preStatesAndSignals OldestFirst tr

  initialState = initialStateNoUTxO { cvsUtxo = initialUTxO }

  initialStateNoUTxO =
    either (panic . show) identity $ initialChainValidationState config

  config = abEnvToCfg abstractEnv

  abstractEnv@(_, abstractInitialUTxO, _, _) = tr ^. traceEnv

  (initialUTxO, txIdMap) = elaborateInitialUTxO abstractInitialUTxO


elaborateAndUpdate
  :: Genesis.Config
 -> (ChainValidationState, Map Abstract.TxId Concrete.TxId)
  -> (State CHAIN, Abstract.Block)
  -> Either
       ChainValidationError
       (ChainValidationState, Map Abstract.TxId Concrete.TxId)
elaborateAndUpdate config (cvs, txIdMap) (ast, ab) =
  (, txIdMap') <$> updateBlock config cvs concreteBlock
 where
  (concreteBlock, txIdMap') = elaborateBS txIdMap config dCert cvs ab
  dCert = rcDCert (ab ^. Abstract.bHeader . Abstract.bhIssuer) ast


classifyTransactions :: Trace CHAIN -> PropertyT IO ()
classifyTransactions =
  collect
    . sum
    . fmap (length . Abstract._bUtxo . Abstract._bBody)
    . traceSignals NewestFirst
