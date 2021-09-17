{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
module Test.Cardano.Chain.Block.Model
  ( tests,
    elaborateAndUpdate,
    passConcreteValidation,
    elaborateBlock,
  )
where

import Byron.Spec.Chain.STS.Block (BlockStats (..))
import qualified Byron.Spec.Chain.STS.Block as Abstract
import Byron.Spec.Chain.STS.Rule.BBody
import Byron.Spec.Chain.STS.Rule.Chain
  ( CHAIN,
    ShouldGenDelegation (NoGenDelegation),
    ShouldGenUTxO (NoGenUTxO),
    ShouldGenUpdate (GenUpdate, NoGenUpdate),
    coverInvalidBlockProofs,
    invalidProofsBlockGen,
    isHeaderSizeTooBigFailure,
    sigGenChain,
  )
import Byron.Spec.Chain.STS.Rule.Epoch (sEpoch)
import qualified Byron.Spec.Ledger.Core as AbstractCore
import Byron.Spec.Ledger.Delegation
  ( DSEnv (DSEnv),
    tamperedDcerts,
    _dIStateDelegationMap,
    _dSEnvAllowedDelegators,
    _dSEnvEpoch,
    _dSEnvK,
    _dSEnvSlot,
  )
import qualified Byron.Spec.Ledger.Delegation.Test as Delegation.Test
import Byron.Spec.Ledger.STS.UTXO (UTxOEnv (..))
import Byron.Spec.Ledger.STS.UTXOW (coverUtxoFailure, tamperedTxList)
import Byron.Spec.Ledger.Update
  ( PParams,
    UPIEnv,
    UPIREG,
    UPIState,
    tamperWithUpdateProposal,
    tamperWithVotes,
    _maxBkSz,
    _maxHdrSz,
  )
import qualified Byron.Spec.Ledger.Update.Test as Update.Test
import Cardano.Chain.Block
  ( ABlock,
    BlockValidationMode (..),
    ChainValidationError
      ( ChainValidationBlockTooLarge,
        ChainValidationHeaderTooLarge,
        ChainValidationProofValidationError
      ),
    ChainValidationState (cvsUtxo),
    ProofValidationError
      ( DelegationProofValidationError,
        UTxOProofValidationError,
        UpdateProofValidationError
      ),
    blockHeader,
    blockLength,
    cvsUpdateState,
    headerLength,
    initialChainValidationState,
    updateBlock,
  )
import Cardano.Chain.Common (unBlockCount)
import qualified Cardano.Chain.Genesis as Genesis
import Cardano.Chain.Update
  ( ProtocolParameters,
    ppMaxBlockSize,
    ppMaxHeaderSize,
  )
import Cardano.Chain.Update.Validation.Interface (adoptedProtocolParameters)
import Cardano.Chain.ValidationMode
  ( ValidationMode,
    fromBlockValidationMode,
  )
import Cardano.Prelude hiding (State, state, trace)
import Control.State.Transition
  ( Environment,
    PredicateFailure,
    Signal,
    State,
    TRC (TRC),
    applySTS,
  )
import Control.State.Transition.Generator
  ( SignalGenerator,
    classifyTraceLength,
    invalidTrace,
    ofLengthAtLeast,
    sigGen,
    trace,
  )
import qualified Control.State.Transition.Invalid.Trace as Invalid.Trace
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (NewestFirst, OldestFirst),
    extractValues,
    lastSignal,
    lastState,
    preStatesAndSignals,
    traceEnv,
    traceInit,
    traceSignals,
    _traceEnv,
  )
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog
  ( Gen,
    MonadTest,
    PropertyT,
    TestLimit,
    assert,
    collect,
    cover,
    evalEither,
    failure,
    footnote,
    forAll,
    label,
    property,
    success,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro ((^.))
import Test.Cardano.Chain.Elaboration.Block
  ( AbstractToConcreteIdMaps,
    abEnvToCfg,
    elaborateBS,
    rcDCert,
    transactionIds,
  )
import Test.Cardano.Chain.Elaboration.Keys (elaborateVKeyGenesisHash)
import Test.Cardano.Chain.UTxO.Model (elaborateInitialUTxO)
import Test.Cardano.Prelude
import Test.Options (TSGroup, TSProperty, withTestsTS)
import Prelude (String)

tests :: TSGroup
tests = $$discoverPropArg

-- | Every abstract chain that was generated according to the inference rules,
-- after being elaborated must be validated by the concrete block validator.
ts_prop_generatedChainsAreValidated :: TSProperty
ts_prop_generatedChainsAreValidated =
  withTestsTS 300 $
    property $ do
      let (traceLength, step) = (200 :: Word64, 10 :: Word64)
      tr <- forAll $ trace @CHAIN () traceLength
      classifyTraceLength tr traceLength step
      classifyBlockStats $
        Abstract.chainBlockStats $
          map Abstract.blockStats (traceSignals OldestFirst tr)
      printAdditionalInfoOnFailure tr
      passConcreteValidation tr
  where
    printAdditionalInfoOnFailure :: MonadTest m => Trace CHAIN -> m ()
    printAdditionalInfoOnFailure tr =
      footnote $ "Allowed delegators hashes: " ++ show allowedDelegatorHashes
      where
        allowedDelegatorHashes = elaborateVKeyGenesisHash <$> Set.toList allowedDelegators
        (_, _, allowedDelegators, _, _) = _traceEnv tr

classifyBlockStats ::
  Maybe (BlockStats, BlockStats, BlockStats) ->
  PropertyT IO ()
classifyBlockStats Nothing = return ()
classifyBlockStats (Just (sMin, sMax, sAvg)) = do
  classify' "min # tx" sMin Abstract.blockStatsUtxo
  classify' "max # tx" sMax Abstract.blockStatsUtxo
  classify' "avg # tx" sAvg Abstract.blockStatsUtxo

  classify' "min # dcert" sMin Abstract.blockStatsDCerts
  classify' "max # dcert" sMax Abstract.blockStatsDCerts
  classify' "avg # dcert" sAvg Abstract.blockStatsDCerts

  classify' "min # votes" sMin Abstract.blockStatsUpdVotes
  classify' "max # votes" sMax Abstract.blockStatsUpdVotes
  classify' "avg # votes" sAvg Abstract.blockStatsUpdVotes

  classify' "min # prop" sMin Abstract.blockStatsUpdProp
  classify' "max # prop" sMax Abstract.blockStatsUpdProp
  classify' "avg # prop" sAvg Abstract.blockStatsUpdProp
  where
    classify' :: String -> BlockStats -> (BlockStats -> Word) -> PropertyT IO ()
    classify' l stats f
      | f stats == 0 = label (fromString (l ++ " == 0"))
      | f stats == 1 = label (fromString (l ++ " == 1"))
      | otherwise = label (fromString (l ++ " >  1"))

passConcreteValidation :: MonadTest m => Trace CHAIN -> m ()
passConcreteValidation tr = void $ evalEither result
  where
    ValidationOutput {result} = applyTrace tr

-- | Elaborate an abstract signal into a concrete one, and apply the validators
-- to the elaborated signal and given concrete state. If the signal was
-- validated, return the next state. Otherwise return an error.
elaborateAndUpdate ::
  Genesis.Config ->
  (ChainValidationState, AbstractToConcreteIdMaps) ->
  (State CHAIN, Abstract.Block) ->
  Either
    ChainValidationError
    (ChainValidationState, AbstractToConcreteIdMaps)
elaborateAndUpdate config (cvs, abstractToConcreteIdMaps) (ast, ab) =
  (,abstractToConcreteIdMaps') <$> runReaderT (updateBlock config cvs concreteBlock) vMode
  where
    (concreteBlock, abstractToConcreteIdMaps') = elaborateBS abstractToConcreteIdMaps config dCert cvs ab

    dCert = rcDCert (ab ^. Abstract.bHeader . Abstract.bhIssuer) stableAfter ast

    stableAfter = AbstractCore.BlockCount $ unBlockCount $ Genesis.configK config

    vMode :: ValidationMode
    vMode = fromBlockValidationMode BlockValidation

elaborateBlock ::
  Genesis.Config ->
  ChainValidationState ->
  AbstractToConcreteIdMaps ->
  State CHAIN ->
  Abstract.Block ->
  ABlock ByteString
elaborateBlock
  config
  chainValidationState
  abstractToConcreteIdMaps
  abstractState
  abstractBlock = concreteBlock
    where
      (concreteBlock, _) =
        elaborateBS
          abstractToConcreteIdMaps
          config
          dCert
          chainValidationState
          abstractBlock
        where
          dCert =
            rcDCert
              (abstractBlock ^. Abstract.bHeader . Abstract.bhIssuer)
              stableAfter
              abstractState
            where
              stableAfter =
                AbstractCore.BlockCount $
                  unBlockCount $
                    Genesis.configK config

classifyTransactions :: Trace CHAIN -> PropertyT IO ()
classifyTransactions =
  collect
    . sum
    . fmap (length . Abstract._bUtxo . Abstract._bBody)
    . traceSignals NewestFirst

ts_prop_invalidDelegationCertificatesAreRejected :: TSProperty
ts_prop_invalidDelegationCertificatesAreRejected =
  invalidChainTracesAreRejected 300 delegationFailureProfile coverDcerts
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
                (\body -> body {Abstract._bDCerts = delegationCerts})

            -- This chooses with even probability between manually tweaked
            -- DCerts and goblin-tweaked ones.
            invalidDelegationCerts = tamperedDcerts delegationEnv delegationSt
              where
                delegationEnv =
                  ( DSEnv
                      { _dSEnvAllowedDelegators = allowedDelegators,
                        _dSEnvEpoch = sEpoch sn k,
                        _dSEnvSlot = sn,
                        _dSEnvK = k
                      }
                  )
                (_, _, _, _, delegationSt, _) = st

    -- @mhueschen : this is lifted from adjacent coverage checkers and does not (at least intentionally)
    -- address the TODOs listed below.
    coverDcerts :: [PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()
    -- TODO: Establish a mapping between concrete and abstract errors. See 'coverDelegationRegistration'
    coverDcerts abstractPfs _concretePfs =
      Delegation.Test.coverDelegFailures 1 abstractPfs

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

-- | Test that the invalid chains that are generated according to the given
-- failure profile are rejected. On failure agreement the given function is
-- called using the abstract and concrete predicate failures as arguments.
invalidChainTracesAreRejected ::
  TestLimit ->
  [(Int, SignalGenerator CHAIN)] ->
  ([PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()) ->
  TSProperty
invalidChainTracesAreRejected numberOfTests failureProfile onFailureAgreement =
  withTestsTS numberOfTests $
    property $ do
      let traceLength = 100 :: Word64
      tr <- forAll $ invalidTrace @CHAIN () traceLength failureProfile
      let ValidationOutput {elaboratedConfig, result} =
            applyTrace (Invalid.Trace.validPrefix tr)
      case result of
        Left error -> do
          footnote $ "Expecting a valid prefix but got: " ++ show error
          failure
        Right concreteState ->
          let abstractState = lastState (Invalid.Trace.validPrefix tr)
              block = Invalid.Trace.signal tr
              result' =
                elaborateAndUpdate
                  elaboratedConfig
                  concreteState
                  (abstractState, block)
           in case (Invalid.Trace.errorOrLastState tr, result') of
                (Right _, Right _) ->
                  -- Success: it is possible that the invalid signals generator
                  -- produces a valid signal, since the generator is probabilistic.
                  success
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
          let upiEnv = mkUpiEnv block env st
              upiSt = mkUpiSt st
          uprop <- sigGen @UPIREG upiEnv upiSt
          tamperedUprop <- tamperWithUpdateProposal upiEnv upiSt uprop
          pure
            $! Abstract.updateBody
              block
              (\body -> body {Abstract._bUpdProp = Just tamperedUprop})

    coverUpdateRegistration :: [PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()
    -- TODO: Establish a mapping between concrete and abstract errors. See 'coverDelegationRegistration'
    coverUpdateRegistration abstractPfs _concretePfs =
      Update.Test.coverUpiregFailures 1 abstractPfs

-- | Extract the update interface environment from a given block and chain
-- environment and state.
--
-- TODO: this should be in `cardano-ledger-specs`.
mkUpiEnv ::
  Abstract.Block ->
  Environment CHAIN ->
  State CHAIN ->
  UPIEnv
mkUpiEnv block env st = (blockSlot, _dIStateDelegationMap delegSt, k, ngk)
  where
    blockSlot = Abstract._bhSlot (Abstract._bHeader block)
    (_, _, allowedDelegators, _, k) = env
    (_slot, _sgs, _h, _utxoSt, delegSt, _upiSt) = st
    numberOfDelegators = Set.size allowedDelegators
    ngk
      | fromIntegral (maxBound :: Word8) < numberOfDelegators =
        panic $
          "ts_prop_invalidDelegationSignalsAreRejected: "
            <> "too many genesis keys: "
            <> show numberOfDelegators
      | otherwise = fromIntegral numberOfDelegators

-- | Extract the update state from the given chain state.
--
-- TODO: put this in `cardano-ledger-specs`.
mkUpiSt ::
  State CHAIN ->
  UPIState
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
          utxoEnv = UTxOEnv {utxo0 = utxo, pps = pparams}
      txWitsList <- tamperedTxList utxoEnv utxoSt
      pure
        $! Abstract.updateBody
          block
          (\body -> body {Abstract._bUtxo = txWitsList})

    coverTxWits :: [PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()
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
          let blockVotes = Abstract._bUpdVotes (Abstract._bBody block)
          tamperedVotes <- tamperWithVotes (mkUpiEnv block env st) (mkUpiSt st) blockVotes
          pure
            $! Abstract.updateBody
              block
              (\body -> body {Abstract._bUpdVotes = tamperedVotes})

    coverVotes :: [PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()
    -- TODO: Establish a mapping between concrete and abstract errors. See 'coverDelegationRegistration'
    coverVotes abstractPfs _concretePf =
      Update.Test.coverUpivoteFailures 1 abstractPfs

ts_prop_invalidBlockPayloadProofsAreRejected :: TSProperty
ts_prop_invalidBlockPayloadProofsAreRejected =
  invalidChainTracesAreRejected 300 [(1, invalidProofsBlockGen)] coverFailures
  where
    coverFailures :: [PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()
    coverFailures abstractPfs concretePf = do
      coverInvalidBlockProofs 15 abstractPfs
      -- Check that the concrete failures correspond with the abstract ones.
      when (any (== InvalidDelegationHash) $ extractValues abstractPfs) $
        assert $ concretePf == ChainValidationProofValidationError DelegationProofValidationError
      when (any (== InvalidUpdateProposalHash) $ extractValues abstractPfs) $
        assert $ concretePf == ChainValidationProofValidationError UpdateProofValidationError
      when (any (== InvalidUtxoHash) $ extractValues abstractPfs) $
        assert $ concretePf == ChainValidationProofValidationError UTxOProofValidationError

-- | Output resulting from elaborating and validating an abstract trace with
-- the concrete validators.
data ValidationOutput = ValidationOutput
  { -- | Elaborated configuration. This configuration results from elaborating
    -- the trace initial environment.
    elaboratedConfig :: !Genesis.Config,
    result ::
      !( Either
           ChainValidationError
           (ChainValidationState, AbstractToConcreteIdMaps)
       )
  }

-- | Apply the concrete validators to the given abstract trace.
applyTrace ::
  Trace CHAIN ->
  ValidationOutput
applyTrace tr =
  ValidationOutput
    { elaboratedConfig = config,
      result =
        foldM (elaborateAndUpdate config) (initialState, initialAbstractToConcreteIdMaps) $
          preStatesAndSignals OldestFirst tr
    }
  where
    initialState = initialStateNoUTxO {cvsUtxo = initialUTxO}

    initialAbstractToConcreteIdMaps = mempty {transactionIds = txIdMap}

    initialStateNoUTxO =
      either (panic . show) identity $ initialChainValidationState config

    config = abEnvToCfg abstractEnv

    abstractEnv@( _currentSlot,
                  abstractInitialUTxO,
                  _allowedDelegators,
                  _protocolParams,
                  _stableAfter
                  ) = tr ^. traceEnv

    (initialUTxO, txIdMap) = elaborateInitialUTxO abstractInitialUTxO

ts_prop_invalidHeaderSizesAreRejected :: TSProperty
ts_prop_invalidHeaderSizesAreRejected =
  invalidSizesAreRejected
    (\pps newMax -> pps {_maxHdrSz = newMax})
    (Abstract.bHeaderSize . Abstract._bHeader)
    (\protocolParameters newMax -> protocolParameters {ppMaxHeaderSize = newMax})
    (headerLength . blockHeader)
    checkMaxSizeFailure
  where
    checkMaxSizeFailure ::
      [PredicateFailure CHAIN] ->
      ChainValidationError ->
      PropertyT IO ()
    checkMaxSizeFailure abstractPfs ChainValidationHeaderTooLarge {} = do
      assert $
        any isHeaderSizeTooBigFailure abstractPfs
      footnote $
        "HeaderSizeTooBig not found in the abstract predicate failures: "
          ++ show abstractPfs
    checkMaxSizeFailure _ concretePF = do
      footnote $ "Expected 'ChainValidationHeaderTooLarge' error, got " ++ show concretePF
      failure

-- | Check that blocks with components (e.g. headers, bodies) that have an
-- invalid size (according to the protocol parameters) are rejected.
invalidSizesAreRejected ::
  -- | Setter for the abstract protocol parameters. The 'Natural' parameter is
  -- used to pass a new (generated) maximum size.
  (PParams -> Natural -> PParams) ->
  -- | Function used to compute the size of the abstract-block's component.
  (Abstract.Block -> Natural) ->
  -- | Setter for the concrete protocol parameters.
  (ProtocolParameters -> Natural -> ProtocolParameters) ->
  -- | Function used to compute the size of the concrete-block's component.
  (ABlock ByteString -> Natural) ->
  -- | Function to check agreement of concrete and abstract failures.
  ([PredicateFailure CHAIN] -> ChainValidationError -> PropertyT IO ()) ->
  TSProperty
invalidSizesAreRejected
  setAbstractParamTo
  abstractBlockComponentSize
  setConcreteParamTo
  concreteBlockComponentSize
  checkFailures =
    withTestsTS 300 $
      property $ do
        tr <- forAll $ trace @CHAIN () 100 `ofLengthAtLeast` 1
        let ValidationOutput {elaboratedConfig, result} =
              applyTrace initTr
            initTr :: Trace CHAIN
            initTr = traceInit tr
        case result of
          Left error -> do
            footnote $ "Expecting a valid trace but got: " ++ show error
            failure
          Right (concreteState, concreteIdMaps) -> do
            let abstractState :: State CHAIN
                abstractState = lastState $ initTr

                lastBlock :: Signal CHAIN
                lastBlock = lastSignal tr

                abstractEnv :: Environment CHAIN
                abstractEnv = _traceEnv tr

                concreteLastBlock =
                  -- NOTE: since we're altering the maximum header/body size defined in
                  -- the parameters we it shouldn't matter whether we're using the
                  -- altered states.
                  elaborateBlock
                    elaboratedConfig
                    concreteState
                    concreteIdMaps
                    abstractState
                    lastBlock

                lastBlockHeaderSize = abstractBlockComponentSize lastBlock

                concreteLastBlockSize = concreteBlockComponentSize concreteLastBlock

            alteredAbstractState :: State CHAIN <-
              forAll $ genAbstractAlteredState abstractState (lastBlockHeaderSize - 1)

            alteredConcreteState <-
              forAll $ genConcreteAlteredState concreteState (concreteLastBlockSize - 1)
            --
            -- NOTE: Here we could simply choose not to use the concrete block size
            -- and use the abstract block size instead. The former will be larger
            -- than the latter. However the problem will be that we won't be
            -- testing the boundary condition block size - 1 == maximum block size

            let abstractResult =
                  applySTS @CHAIN (TRC (abstractEnv, alteredAbstractState, lastBlock))

                concreteResult =
                  elaborateAndUpdate
                    elaboratedConfig
                    (alteredConcreteState, concreteIdMaps)
                    (alteredAbstractState, lastBlock)

            case (abstractResult, concreteResult) of
              (Left abstractPfs, Left concretePf) -> do
                checkFailures abstractPfs concretePf
                cover 85 "Size validation hit" True
                success
              (Right _, Right _) ->
                -- It might be that we are at an epoch boundary, so the altered
                -- update state will be overwritten by an epoch transition. In that
                -- case it is OK if both executable spec and implementation return
                -- 'Right'.
                success
              _ -> do
                footnote "Validation results mismatch."
                footnote $ "Altered abstract state: " ++ show alteredAbstractState
                footnote $ "Signal: " ++ show lastBlock
                footnote $ "Abstract result: " ++ show abstractResult
                footnote $ "Concrete result: " ++ show concreteResult
                failure
    where
      genAbstractAlteredState :: State CHAIN -> Natural -> Gen (State CHAIN)
      genAbstractAlteredState (slot, sgs, h, utxo, ds, us) maxSize =
        (slot,sgs,h,utxo,ds,) <$> genAlteredUpdateState us
        where
          genAlteredUpdateState ((pv, pps), fads, avs, rpus, raus, cps, vts, bvs, pws) = do
            newMaxSize <- Gen.integral (Range.constant 0 maxSize)
            pure
              $! ( (pv, pps `setAbstractParamTo` newMaxSize),
                   fads,
                   avs,
                   rpus,
                   raus,
                   cps,
                   vts,
                   bvs,
                   pws
                 )

      genConcreteAlteredState ::
        ChainValidationState -> Natural -> Gen ChainValidationState
      genConcreteAlteredState state maxSize =
        setMaxHeaderSize <$> Gen.integral (Range.constant 0 maxSize)
        where
          setMaxHeaderSize newSize = state {cvsUpdateState = newUpdateState}
            where
              newUpdateState =
                (cvsUpdateState state) {adoptedProtocolParameters = newParameters}
                where
                  newParameters =
                    (adoptedProtocolParameters (cvsUpdateState state))
                      `setConcreteParamTo` newSize

ts_prop_invalidBlockSizesAreRejected :: TSProperty
ts_prop_invalidBlockSizesAreRejected =
  invalidSizesAreRejected
    (\pps newMax -> pps {_maxBkSz = newMax})
    Abstract.bSize
    (\protocolParameters newMax -> protocolParameters {ppMaxBlockSize = newMax})
    blockLength
    checkMaxSizeFailure
  where
    checkMaxSizeFailure ::
      [PredicateFailure CHAIN] ->
      ChainValidationError ->
      PropertyT IO ()
    checkMaxSizeFailure abstractPfs ChainValidationBlockTooLarge {} = do
      assert $
        any (== InvalidBlockSize) $ extractValues abstractPfs
      footnote $
        "InvalidBlockSize not found in the abstract predicate failures: "
          ++ show abstractPfs
    checkMaxSizeFailure _ concretePF = do
      footnote $ "Expected 'ChainValidationBlockTooLarge' error, got " ++ show concretePF
      failure
