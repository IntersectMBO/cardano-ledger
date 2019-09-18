{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , TestLimit
  , collect
  , evalEither
  , failure
  , footnote
  , forAll
  , property
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

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
import Cardano.Ledger.Spec.STS.UTXO (UTxOEnv(..))
import Cardano.Ledger.Spec.STS.UTXOW (coverUtxoFailure, tamperedTxWitsList)
import Cardano.Spec.Chain.STS.Rule.Chain
  ( CHAIN
  , ShouldGenDelegation(NoGenDelegation)
  , ShouldGenUTxO(NoGenUTxO)
  , ShouldGenUpdate(NoGenUpdate, GenUpdate)
  , sigGenChain
  )
import           Cardano.Spec.Chain.STS.Rule.Epoch (sEpoch)
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import Control.State.Transition.Generator (classifyTraceLength, trace, invalidTrace, SignalGenerator, sigGen)
import Control.State.Transition (State, Environment, PredicateFailure)
import qualified Control.State.Transition.Invalid.Trace as Invalid.Trace
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(NewestFirst, OldestFirst)
  , _traceEnv
  , lastState
  , preStatesAndSignals
  , traceEnv
  , traceSignals
  )
import qualified Ledger.Core as AbstractCore
import Ledger.Delegation
  ( DSEnv (DSEnv)
  , _dSEnvAllowedDelegators
  , _dIStateDelegationMap
  , _dSEnvEpoch
  , _dSEnvK
  , _dSEnvSlot
  , randomDCertGen
  )
import Ledger.Update (tamperWithUpdateProposal, UPIREG, UPIEnv, tamperWithVotes, UPIState)
import qualified Ledger.Update.Test as Update.Test

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
  withTestsTS 300  $ property $ do
    let (traceLength, step) = (200 :: Word64, 10 :: Word64)
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
passConcreteValidation tr = void $ evalEither result
 where
   ValidationOutput { result } = applyTrace tr


-- | Elaborate an abstract signal into a concrete one, and apply the validators
-- to the elaborated signal and given concrete state. If the signal was
-- validated, return the next state. Otherwise return an error.
--
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


ts_prop_invalidDelegationCertificatesAreRejected :: TSProperty
ts_prop_invalidDelegationCertificatesAreRejected =
  invalidChainTracesAreRejected 300 delegationFailureProfile coverDelegationRegistration
  where
    delegationFailureProfile :: [(Int, SignalGenerator CHAIN)]
    delegationFailureProfile = [(1, invalidDelegationGen)]
      where
        invalidDelegationGen :: SignalGenerator CHAIN
        invalidDelegationGen env@(sn, _, allowedDelegators, _, k) st =
          addDelegation <$> sigGenChain NoGenDelegation NoGenUTxO NoGenUpdate env st
                        <*> invalidDelegationCerts
          where
            addDelegation block delegationCerts =
              Abstract.updateBody
                block
                (\body -> body { Abstract._bDCerts = delegationCerts })

            invalidDelegationCerts = Gen.list (Range.constant 0 10)
                                              (randomDCertGen delegationEnv)
              where
                delegationEnv =
                  ( DSEnv
                    { _dSEnvAllowedDelegators = allowedDelegators
                    , _dSEnvEpoch = sEpoch sn k
                    , _dSEnvSlot = sn
                    , _dSEnvK = k
                    }
                  )


    coverDelegationRegistration :: [[PredicateFailure CHAIN]]  -> ChainValidationError -> PropertyT IO ()
    -- TODO: add coverage testing for delegation.
    --
    -- TODO: we could establish a mapping between concrete and abstract errors.
    --
    -- TODO: we want to check that the concrete errors are included in
    -- the abstract ones. Currently the concrete implementation
    -- short-circuits on the first failure, that's why we can only
    -- check inclusion at the moment. To make sure we cover the errors
    -- that can arise in the concrete implementation we need to make
    -- sure that the invalid generators cover a good deal of abstract
    -- errors permutations. So for instance, given abstract errors @X@,
    -- @Y@, and @Z@, we would want to generate all combinations of them.
    coverDelegationRegistration _abstractPfs _concretePfs = pure ()


-- | Test that the invalid chains that are generated according to the given
-- failure profile are rejected. On failure agreement the given function is
-- called using the abstract and concrete predicate failures as arguments.
invalidChainTracesAreRejected
  :: TestLimit
  -> [(Int, SignalGenerator CHAIN)]
  -> ([[PredicateFailure CHAIN]]  -> ChainValidationError -> PropertyT IO ())
  -> TSProperty
invalidChainTracesAreRejected numberOfTests failureProfile onFailureAgreement =
  withTestsTS numberOfTests $ property $ do
    let traceLength = 100 :: Word64
    tr <- forAll $ invalidTrace @CHAIN traceLength failureProfile
    let ValidationOutput { elaboratedConfig, result } =
          applyTrace (Invalid.Trace.validPrefix tr)
    case result of
      Left error -> do
        footnote $ "Expecting a valid prefix but got: " ++ show error
        failure
      Right concreteState ->
        let abstractState = lastState (Invalid.Trace.validPrefix tr)
            block = Invalid.Trace.signal tr
            result' = elaborateAndUpdate
                    elaboratedConfig
                    concreteState
                    (abstractState, block)
        in
        case (Invalid.Trace.errorOrLastState tr, result') of
          (Right _, Right _) ->
            -- Success: it is possible that the invalid signals generator
            -- produces a valid signal, since the generator is probabilistic.
            pure ()
          (Left abstractPfs, Left concretePfs) -> do
            -- Success: both the model and the concrete implementation failed
            -- to validate the signal.
            --
            onFailureAgreement abstractPfs concretePfs
          (abstractResult, concreteResult) -> do
            footnote "Validation results mismatch."
            footnote $ "Signal: " ++ show block
            footnote $ "Abstract result: " ++ show abstractResult
            footnote $ "Concrete result: " ++ show concreteResult
            failure


ts_prop_invalidUpdateRegistrationsAreRejected :: TSProperty
ts_prop_invalidUpdateRegistrationsAreRejected =
  invalidChainTracesAreRejected 300 updateRegistrationFailureProfile coverUpdateRegistration
  where
    updateRegistrationFailureProfile :: [(Int, SignalGenerator CHAIN)]
    updateRegistrationFailureProfile = [(1, invalidUpdateProposalGen)]
      where
        invalidUpdateProposalGen :: SignalGenerator CHAIN
        invalidUpdateProposalGen env st = do
          block <- sigGenChain NoGenDelegation NoGenUTxO NoGenUpdate env st
          let
            upiEnv = mkUpiEnv block env st
            upiSt = mkUpiSt st
          uprop <- sigGen @UPIREG upiEnv upiSt
          tamperedUprop <- tamperWithUpdateProposal upiEnv upiSt uprop
          pure $! Abstract.updateBody
                    block
                    (\body -> body { Abstract._bUpdProp = Just tamperedUprop })

    coverUpdateRegistration :: [[PredicateFailure CHAIN]]  -> ChainValidationError -> PropertyT IO ()
    -- TODO: Establish a mapping between concrete and abstract errors. See 'coverDelegationRegistration'
    coverUpdateRegistration abstractPfs _concretePfs =
      Update.Test.coverUpiregFailures 1 abstractPfs


-- | Extract the update interface environment from a given block and chain
-- environment and state.
--
-- TODO: this should be in `cardano-ledger-specs`.
--
mkUpiEnv
  :: Abstract.Block
  -> Environment CHAIN
  -> State CHAIN
  -> UPIEnv
mkUpiEnv block env st = (blockSlot, _dIStateDelegationMap delegSt, k, ngk)
  where
    blockSlot = Abstract._bhSlot (Abstract._bHeader block)
    (_, _, allowedDelegators, _, k) = env
    (_slot, _sgs, _h, _utxoSt, delegSt, _upiSt) = st
    numberOfDelegators = Set.size allowedDelegators
    ngk
      | fromIntegral (maxBound :: Word8) < numberOfDelegators =
          panic $ "ts_prop_invalidDelegationSignalsAreRejected: "
                <> "too many genesis keys: "
                <> show numberOfDelegators
      | otherwise = fromIntegral numberOfDelegators


-- | Extract the update state from the given chain state.
--
-- TODO: put this in `cardano-ledger-specs`.
--
mkUpiSt
  :: State CHAIN
  -> UPIState
mkUpiSt (_slot, _sgs, _h, _utxoSt, _delegSt, upiSt) = upiSt


ts_prop_invalidTxWitsAreRejected :: TSProperty
ts_prop_invalidTxWitsAreRejected =
  invalidChainTracesAreRejected 300 failureProfile coverTxWits
 where
  failureProfile :: [(Int, SignalGenerator CHAIN)]
  failureProfile = [(1, invalidTxWitsGen)]

  invalidTxWitsGen :: SignalGenerator CHAIN
  invalidTxWitsGen env@(_, utxo, _, pparams, _) st = do
    block <- sigGenChain NoGenDelegation NoGenUTxO NoGenUpdate env st
    let (_slot, _sgs, _h, utxoSt, _delegSt, _upiSt) = st
        utxoEnv = UTxOEnv { utxo0 = utxo, pps = pparams }
    txWitsList <- tamperedTxWitsList utxoEnv utxoSt
    pure $! Abstract.updateBody
              block
              (\body -> body { Abstract._bUtxo = txWitsList })

  coverTxWits :: [[PredicateFailure CHAIN]]  -> ChainValidationError -> PropertyT IO ()
  -- TODO: Establish a mapping between concrete and abstract errors. See 'coverDelegationRegistration'
  coverTxWits abstractPfs _concretePfs =
    coverUtxoFailure 1 abstractPfs

ts_prop_invalidVotesAreRejected :: TSProperty
ts_prop_invalidVotesAreRejected =
  invalidChainTracesAreRejected 300 votesFailureProfile coverVotes
  where
    votesFailureProfile :: [(Int, SignalGenerator CHAIN)]
    votesFailureProfile = [(1, invalidVotesGen)]
      where
        invalidVotesGen :: SignalGenerator CHAIN
        invalidVotesGen env st = do
          block <- sigGenChain NoGenDelegation NoGenUTxO GenUpdate env st
          let
            blockVotes = Abstract._bUpdVotes (Abstract._bBody block)
          tamperedVotes <- tamperWithVotes (mkUpiEnv block env st) (mkUpiSt st) blockVotes
          pure $! Abstract.updateBody
                    block
                    (\body -> body { Abstract._bUpdVotes = tamperedVotes })

    coverVotes :: [[PredicateFailure CHAIN]]  -> ChainValidationError -> PropertyT IO ()
    -- TODO: Establish a mapping between concrete and abstract errors. See 'coverDelegationRegistration'
    coverVotes abstractPfs _concretePfs =
      Update.Test.coverUpivoteFailures 1 abstractPfs


-- | Output resulting from elaborating and validating an abstract trace with
-- the concrete validators.
data ValidationOutput = ValidationOutput
  { elaboratedConfig :: !Genesis.Config
  -- ^ Elaborated configuration. This configuration results from elaborating
  -- the trace initial environment.
  , result :: !(Either
                  ChainValidationError
                  (ChainValidationState, AbstractToConcreteIdMaps)
               )
  }


-- | Apply the concrete validators to the given abstract trace.
--
applyTrace
  :: Trace CHAIN
  -> ValidationOutput
applyTrace tr =
  ValidationOutput
  { elaboratedConfig = config
  , result = foldM (elaborateAndUpdate config) (initialState, initialAbstractToConcreteIdMaps)
           $ preStatesAndSignals OldestFirst tr
  }
  where
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
