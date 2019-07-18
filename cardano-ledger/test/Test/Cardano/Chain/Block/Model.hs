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
  , elaborateAndUpdate
  , passConcreteValidation
  )
where

import Cardano.Prelude hiding (trace, State)
import Test.Cardano.Prelude

import Control.Lens ((^.))
import qualified Data.Set as Set
import Data.Word (Word64)
import Hedgehog
  ( MonadTest
  , PropertyT
  , collect
  , evalEither
  , footnote
  , forAll
  , property
  )

import Cardano.Chain.Block
  ( BlockValidationMode (..)
  , ChainValidationError
  , ChainValidationState(cvsUtxo)
  , initialChainValidationState
  , updateBlock
  )
import Cardano.Chain.Common (unBlockCount)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.ValidationMode
  (ValidationMode, fromBlockValidationMode)
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import Control.State.Transition.Generator (classifyTraceLength, trace)
import Control.State.Transition (State)
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(NewestFirst, OldestFirst)
  , preStatesAndSignals
  , traceEnv
  , _traceEnv
  , traceSignals
  )
import qualified Ledger.Core as AbstractCore

import Test.Cardano.Chain.Elaboration.Block
  ( AbstractToConcreteIdMaps
  , abEnvToCfg
  , elaborateBS
  , rcDCert
  , transactionIds
  )
import Test.Cardano.Chain.Elaboration.Keys (elaborateVKeyGenesisHash)
import Test.Cardano.Chain.UTxO.Model (elaborateInitialUTxO)
import Test.Options (TSGroup, TSProperty, withTestsTS)


tests :: TSGroup
tests = $$discoverPropArg


-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
ts_prop_generatedChainsAreValidated :: TSProperty
ts_prop_generatedChainsAreValidated =
  withTestsTS 100 $ property $ do
    let (traceLength, step) = (100 :: Word64, 10 :: Word64)
    tr <- forAll $ trace @CHAIN traceLength
    classifyTraceLength tr traceLength step
    printAdditionalInfoOnFailure tr
    passConcreteValidation tr
  where
    printAdditionalInfoOnFailure :: MonadTest m => Trace CHAIN -> m ()
    printAdditionalInfoOnFailure tr =
      footnote $ "Allowed delegators hashes: " ++ show allowedDelegatorHashes
      where
        allowedDelegatorHashes = elaborateVKeyGenesisHash <$> Set.toList allowedDelegators
        (_, _, allowedDelegators, _, _) = _traceEnv tr


passConcreteValidation :: MonadTest m => Trace CHAIN -> m ()
passConcreteValidation tr = void $ evalEither res
 where
  res =
    foldM (elaborateAndUpdate config) (initialState, initialAbstractToConcreteIdMaps)
      $ preStatesAndSignals OldestFirst tr

  initialState = initialStateNoUTxO { cvsUtxo = initialUTxO }

  initialAbstractToConcreteIdMaps = mempty { transactionIds = txIdMap }

  initialStateNoUTxO =
    either (panic . show) identity $ initialChainValidationState config

  config = abEnvToCfg abstractEnv

  abstractEnv@( _currentSlot
              , abstractInitialUTxO
              , _allowedDelegators
              , _protocolParams
              , _stableAfter) = tr ^. traceEnv

  (initialUTxO, txIdMap) = elaborateInitialUTxO abstractInitialUTxO


elaborateAndUpdate
  :: Genesis.Config
  -> (ChainValidationState, AbstractToConcreteIdMaps)
  -> (State CHAIN, Abstract.Block)
  -> Either
       ChainValidationError
       (ChainValidationState, AbstractToConcreteIdMaps)
elaborateAndUpdate config (cvs, abstractToConcreteIdMaps) (ast, ab) =
  (, abstractToConcreteIdMaps') <$> runReaderT (updateBlock config cvs concreteBlock) vMode
 where
  (concreteBlock, abstractToConcreteIdMaps') = elaborateBS abstractToConcreteIdMaps config dCert cvs ab

  dCert = rcDCert (ab ^. Abstract.bHeader . Abstract.bhIssuer) stableAfter ast

  stableAfter = AbstractCore.BlockCount $ unBlockCount $ Genesis.configK config

  vMode :: ValidationMode
  vMode = fromBlockValidationMode BlockValidation


classifyTransactions :: Trace CHAIN -> PropertyT IO ()
classifyTransactions =
  collect
    . sum
    . fmap (length . Abstract._bUtxo . Abstract._bBody)
    . traceSignals NewestFirst
