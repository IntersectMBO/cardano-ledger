{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Byron.Spec.Ledger.Update.Properties
  ( upiregTracesAreClassified,
    upiregRelevantTracesAreCovered,
    onlyValidSignalsAreGenerated,
    ublockTraceLengthsAreClassified,
    ublockOnlyValidSignalsAreGenerated,
    ublockRelevantTracesAreCovered,
    ublockSampleTraceMetrics,
    invalidRegistrationsAreGenerated,
    invalidSignalsAreGenerated,
  )
where

import Byron.Spec.Ledger.Core
  ( BlockCount (BlockCount),
    Slot (Slot),
    SlotCount (SlotCount),
    dom,
    unBlockCount,
  )
import qualified Byron.Spec.Ledger.Core as Core
import Byron.Spec.Ledger.GlobalParams (slotsPerEpoch)
import Byron.Spec.Ledger.Update
  ( PParams,
    ProtVer,
    UPIEND,
    UPIEnv,
    UPIREG,
    UPIState,
    UPIVOTES,
    UProp,
    Vote,
    emptyUPIState,
    protocolParameters,
    tamperWithUpdateProposal,
    tamperWithVotes,
  )
import qualified Byron.Spec.Ledger.Update as Update
import qualified Byron.Spec.Ledger.Update.Test as Update.Test
import Control.State.Transition
  ( Embed,
    Environment,
    IRC (IRC),
    PredicateFailure,
    STS,
    Signal,
    State,
    TRC (TRC),
    initialRules,
    judgmentContext,
    trans,
    transitionRules,
    wrapFailed,
    (?!),
  )
import Control.State.Transition.Generator
  ( HasTrace,
    SignalGenerator,
    envGen,
    randomTraceOfSize,
    ratio,
    sigGen,
    trace,
    traceLengthsAreClassified,
    traceOfLength,
  )
import qualified Control.State.Transition.Generator as Transition.Generator
import Control.State.Transition.Trace
  ( Trace,
    TraceOrder (OldestFirst),
    traceLength,
    traceSignals,
    traceStates,
    _traceEnv,
    _traceInitState,
  )
import qualified Data.Bimap as Bimap
import Data.Data (Data, Typeable)
import Data.Foldable (fold, traverse_)
import Data.Function ((&))
import Data.List.Unique (count)
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Stack (HasCallStack)
import Hedgehog (Property, cover, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

upiregTracesAreClassified :: Property
upiregTracesAreClassified =
  withTests 100 $ traceLengthsAreClassified @UPIREG () 100 10

upiregRelevantTracesAreCovered :: Property
upiregRelevantTracesAreCovered = withTests 300 $
  property $ do
    sample <- forAll (trace @UPIREG () 200)

    cover
      40
      "at least 30% of the update proposals increase the major version"
      (0.25 <= ratio increaseMajor sample)

    cover
      40
      "at least 30% of the update proposals increase the minor version"
      (0.25 <= ratio increaseMinor sample)

    cover
      30
      "at least 10% of the update proposals do not change the protocol version"
      (0.10 <= ratio dontChangeProtocolVersion sample)

    -- We have a roughly fair distribution of issuers.
    --
    -- Here we simply compute how many update proposals a given issuer should
    -- have made if everybody made the same number of proposals, and check that
    -- each issuer didn't produce less than a certain percentage of that. So for
    -- instance, if we have 4 genesis keys and a trace length of 100, then in an
    -- completely fair distribution of issuers we would expect to have 25 update
    -- proposals per-genesis key. So in this test we would check that in 50% of
    -- the generated traces, the deviation from the fair distribution (25 update
    -- proposals per-key) is no greater than half its expected value (25 * 0.5 =
    -- 12.5).
    cover
      50
      "the distribution of the proposal issuers is roughly fair"
      ( safeMaximum (issuersDeviationsWrtUniformDistribution sample)
          <= expectedNumberOfUpdateProposalsPerKey sample * 0.5
      )

    --------------------------------------------------------------------------------
    -- Maximum block-size checks
    --------------------------------------------------------------------------------
    -- NOTE: since we want to generate valid signals, we cannot decrease the block
    -- size in most of the cases.
    cover
      20
      "at least 5% of the update proposals decrease the maximum block-size"
      (0.05 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz Decreases) sample)

    cover
      50
      "at least 30% of the update proposals increase the maximum block-size"
      (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz Increases) sample)

    cover
      30
      "at least 10% of the update proposals do not change the maximum block-size"
      (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz RemainsTheSame) sample)

    --------------------------------------------------------------------------------
    -- Maximum header-size checks
    --------------------------------------------------------------------------------
    cover
      20
      "at least 5% of the update proposals decrease the maximum header-size"
      (0.05 <= ratio (wrtCurrentProtocolParameters Update._maxHdrSz Decreases) sample)

    cover
      50
      "at least 30% of the update proposals increase the maximum header-size"
      (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxHdrSz Increases) sample)

    cover
      50
      "at least 10% of the update proposals do not change the maximum header-size"
      (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxHdrSz RemainsTheSame) sample)

    --------------------------------------------------------------------------------
    -- Maximum transaction-size checks
    --------------------------------------------------------------------------------
    cover
      20
      "at least 5% of the update proposals decrease the maximum transaction-size"
      (0.05 <= ratio (wrtCurrentProtocolParameters Update._maxTxSz Decreases) sample)

    cover
      50
      "at least 30% of the update proposals increase the maximum transaction-size"
      (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxTxSz Increases) sample)

    cover
      40
      "at least 10% of the update proposals do not change the maximum transaction-size"
      (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxTxSz RemainsTheSame) sample)

    --------------------------------------------------------------------------------
    -- Maximum proposal-size checks
    --------------------------------------------------------------------------------
    cover
      20
      "at least 30% of the update proposals decrease the maximum proposal-size"
      (0.05 <= ratio (wrtCurrentProtocolParameters Update._maxPropSz Decreases) sample)

    cover
      50
      "at least 30% of the update proposals increase the maximum proposal-size"
      (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxPropSz Increases) sample)

    cover
      50
      "at least 10% of the update proposals do not change the maximum proposal-size"
      (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxPropSz RemainsTheSame) sample)
  where
    -- NOTE: after empirically determining the checks above are sensible, we can
    -- add more coverage tests for the other protocol parameters.

    increaseMajor :: Trace UPIREG -> Int
    increaseMajor traceSample =
      protocolVersions traceSample
        & fmap Update._pvMaj
        & filter (currPvMaj traceSample <)
        & length

    protocolVersions :: Trace UPIREG -> [Update.ProtVer]
    protocolVersions tr =
      fmap Update._upPV (traceSignals OldestFirst tr)

    -- We can take the current protocol version from the initial state of
    -- UPIREG, since this transition does not change the protocol version.
    currPvMaj :: Trace UPIREG -> Natural
    currPvMaj = Update._pvMaj . Update.protocolVersion . _traceInitState

    currentProtocolVersion :: Trace UPIREG -> Update.ProtVer
    currentProtocolVersion = Update.protocolVersion . _traceInitState

    increaseMinor :: Trace UPIREG -> Int
    increaseMinor traceSample =
      protocolVersions traceSample
        & fmap Update._pvMin
        & filter (currPvMaj traceSample <)
        & length

    dontChangeProtocolVersion :: Trace UPIREG -> Int
    dontChangeProtocolVersion traceSample =
      protocolVersions traceSample
        & filter (currentProtocolVersion traceSample ==)
        & length

    -- Given a accessor function for the protocol parameters, count the number
    -- of update proposals that change the parameter value in the way specified
    -- by the 'Change' parameter.
    wrtCurrentProtocolParameters ::
      Ord v =>
      (PParams -> v) ->
      Change ->
      Trace UPIREG ->
      Int
    wrtCurrentProtocolParameters parameterValue parameterValueChange traceSample =
      fmap (parameterValue . Update._upParams) (traceSignals OldestFirst traceSample)
        & filter (check parameterValueChange)
        & length
      where
        currentParameterValue = parameterValue . protocolParameters . _traceInitState $ traceSample
        check Increases proposedParameterValue = currentParameterValue < proposedParameterValue
        check Decreases proposedParameterValue = proposedParameterValue < currentParameterValue
        check RemainsTheSame proposedParameterValue = currentParameterValue == proposedParameterValue

    -- TODO: leaving this here as it might be useful in the future. Remove if it
    -- isn't (dnadales - 07/17/2019). We use git, but nobody will see if it sits
    -- in our history.
    --
    -- Count the number of times in the sequence of update proposals that the
    -- given protocol value is set to the given value.
    --
    -- Example usage:
    --
    -- > cover 20
    -- >    "at least 5% of the update proposals set the maximum proposal-size to 0"
    -- > (0.05 <= ratio (Update._maxPropSz `isSetTo` 0) sample)
    -- isSetTo
    --   :: Eq v
    --   => (PParams -> v)
    --   -> v
    --   -> Trace UPIREG
    --   -> Int
    -- isSetTo parameterValue value traceSample
    --   = fmap (parameterValue . Update._upParams) (traceSignals OldestFirst traceSample)
    --   & filter (value ==)
    --   & length

    expectedNumberOfUpdateProposalsPerKey :: Trace UPIREG -> Double
    expectedNumberOfUpdateProposalsPerKey traceSample =
      if numberOfGenesisKeys == 0
        then 0
        else fromIntegral $ traceLength traceSample `div` numberOfGenesisKeys
      where
        numberOfGenesisKeys :: Int
        numberOfGenesisKeys =
          length $
            Bimap.keys $
              delegationMap (_traceEnv traceSample)

    issuersDeviationsWrtUniformDistribution ::
      Trace UPIREG ->
      [Double]
    issuersDeviationsWrtUniformDistribution traceSample =
      fmap Update._upIssuer (traceSignals OldestFirst traceSample)
        & count
        -- Take the list of (issuer, count) pairs and keep the count only.
        & fmap snd
        & fmap deviationFromExpectation
      where
        deviationFromExpectation :: Int -> Double
        deviationFromExpectation numberOfProposalsPerKey =
          abs (fromIntegral numberOfProposalsPerKey - expectedNumberOfUpdateProposalsPerKey traceSample)

    delegationMap (_, dms, _, _) = dms

    safeMaximum xs = if null xs then 0 else maximum xs

-- | Change of a value w.r.t. some other value.
data Change = Increases | Decreases | RemainsTheSame

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 300 $ Transition.Generator.onlyValidSignalsAreGenerated @UPIREG () 100

-- | Dummy transition system to test blocks with update payload only.
data UBLOCK deriving (Data, Typeable)

-- | An update block
data UBlock = UBlock
  { issuer :: Core.VKey,
    blockVersion :: ProtVer,
    slot :: Core.Slot,
    optionalUpdateProposal :: Maybe UProp,
    votes :: [Vote]
  }
  deriving (Eq, Show)

-- | Update block state
data UBlockState = UBlockState
  { upienv :: UPIEnv,
    upistate :: UPIState
  }
  deriving (Eq, Show)

data UBlockPredicateFailure
  = UPIREGFailure (PredicateFailure UPIREG)
  | UPIVOTESFailure (PredicateFailure UPIVOTES)
  | UPIENDFailure (PredicateFailure UPIEND)
  | NotIncreasingBlockSlot
  deriving (Eq, Show, Data, Typeable)

instance STS UBLOCK where
  type Environment UBLOCK = UPIEnv

  type State UBLOCK = UBlockState

  type Signal UBLOCK = UBlock

  type PredicateFailure UBLOCK = UBlockPredicateFailure

  initialRules =
    [ do
        IRC env <- judgmentContext
        let (_, _, k, _) = env
            -- TODO: I (dnadales) should have used records for the UPIState :/
            ((pv, pps), fads, avs, rpus, raus, cps, vts, bvs, pws) = emptyUPIState
        -- We overwrite 'upTtl' in the UBLOCK initial rule, so that we have a system where update
        -- proposals can live long enough to allow for confirmation and consideration for
        -- adoption. We set the time to live of a proposal to half the number of slots in an
        -- epoch, which is about the same value we use in production.
        pure
          UBlockState
            { upienv = env,
              upistate =
                ( (pv, pps {Update._upTtl = SlotCount $ slotsPerEpoch k `div` 2}),
                  fads,
                  avs,
                  rpus,
                  raus,
                  cps,
                  vts,
                  bvs,
                  pws
                )
            }
    ]

  transitionRules =
    [ do
        TRC (_, UBlockState {upienv, upistate}, ublock) <- judgmentContext
        let (sn, dms, k, ngk) = upienv
        sn < slot ublock ?! NotIncreasingBlockSlot
        upistateAfterRegistration <-
          case optionalUpdateProposal ublock of
            Nothing -> pure upistate
            Just updateProposal ->
              trans @UPIREG $ TRC (upienv, upistate, updateProposal)
        upistateAfterVoting <-
          trans @UPIVOTES $ TRC (upienv, upistateAfterRegistration, votes ublock)
        upistateAfterEndorsement <-
          trans @UPIEND $ TRC (upienv, upistateAfterVoting, (blockVersion ublock, issuer ublock))
        pure
          UBlockState
            { upienv = (slot ublock, dms, k, ngk),
              upistate = upistateAfterEndorsement
            }
    ]

instance Embed UPIREG UBLOCK where
  wrapFailed = UPIREGFailure

instance Embed UPIVOTES UBLOCK where
  wrapFailed = UPIVOTESFailure

instance Embed UPIEND UBLOCK where
  wrapFailed = UPIENDFailure

instance HasTrace UBLOCK where
  envGen _ =
    do
      let numberOfGenesisKeys = 7
      dms <- Update.dmapGen numberOfGenesisKeys
      -- We don't want a large value of @k@, otherwise we won't see many
      -- confirmed proposals or epoch changes. The problem here is that the
      -- initial environment does not know anything about the trace size, and
      -- @k@ should be a function of it.
      pure (Slot 0, dms, BlockCount 10, numberOfGenesisKeys)

  sigGen _env UBlockState {upienv, upistate} = do
    (anOptionalUpdateProposal, aListOfVotes) <-
      Update.updateProposalAndVotesGen upienv upistate

    -- Don't shrink the issuer as this won't give us additional insight on a
    -- test failure.
    aBlockIssuer <-
      Gen.prune $
        -- Pick a delegate from the delegation map
        Gen.element $ Bimap.elems (Update.delegationMap upienv)

    aBlockVersion <-
      Update.protocolVersionEndorsementGen upienv upistate

    UBlock
      <$> pure aBlockIssuer
      <*> pure aBlockVersion
      <*> nextSlotGen
      <*> pure anOptionalUpdateProposal
      <*> pure aListOfVotes
    where
      nextSlotGen =
        incSlot
          <$> Gen.frequency
            [ (5, Gen.integral (Range.constant 1 10)),
              (1, Gen.integral (Range.linear 10 (unBlockCount k)))
            ]
        where
          incSlot c = sn `Core.addSlot` Core.SlotCount c
          (sn, _, k, _) = upienv

ublockTraceLengthsAreClassified :: Property
ublockTraceLengthsAreClassified =
  withTests 100 $ traceLengthsAreClassified @UBLOCK () 100 10

ublockOnlyValidSignalsAreGenerated :: HasCallStack => Property
ublockOnlyValidSignalsAreGenerated =
  withTests 300 $ Transition.Generator.onlyValidSignalsAreGenerated @UBLOCK () 100

ublockRelevantTracesAreCovered :: Property
ublockRelevantTracesAreCovered = withTests 300 $
  property $ do
    sample <- forAll (traceOfLength @UBLOCK () 100)

    -- Since we generate votes on the most voted proposals, we do not expect a very large percentage
    -- of confirmed proposals. As a reference, in the runs that were performed manually, for a trace
    -- of 500 blocks, 80-90 update proposals and 20-30 confirmed proposals were observed.
    cover
      75
      "at least 20% of the proposals get confirmed"
      (0.2 <= confirmedProposals sample / totalProposals sample)

    cover
      75
      "at least 30% of the proposals get unconfirmed"
      (0.3 <= 1 - (confirmedProposals sample / totalProposals sample))

    cover
      75
      "at least 2% of the proposals get voted in the same block "
      (0.02 <= fromIntegral (numberOfVotesForBlockProposal sample) / totalProposals sample)

    cover
      75
      "at least 30% of blocks contain no votes"
      (0.3 <= ratio numberOfBlocksWithoutVotes sample)

    -- Once the most voted update proposals get votes from all the genesis keys there is no
    -- possibility of generating any more votes. So we do not expect a large percentage of blocks with
    -- votes.
    cover
      70
      "at least 10% of blocks contain votes"
      (0.1 <= 1 - ratio numberOfBlocksWithoutVotes sample)

    -- We generate an update proposal with a probability 1/4, but we're conservative about the
    -- coverage requirements, since we have variable trace lengths.
    cover
      75
      "at least 70% of the blocks contain no update proposals"
      (0.7 <= ratio numberOfBlocksWithoutUpdateProposals sample)

    cover
      75
      "at least 10% of the blocks contain an update proposals"
      (0.1 <= 1 - ratio numberOfBlocksWithoutUpdateProposals sample)

    -- With traces of length 500, we expect to see about 80-90 proposals, which means that we will
    -- have about 8 to 9 proposals scheduled for adoption.
    cover
      60
      "at least 10% of the proposals get enough endorsements"
      (0.1 <= proposalsScheduledForAdoption sample / totalProposals sample)

    cover
      80
      "at least 5% of the proposals get enough endorsements"
      (0.05 <= proposalsScheduledForAdoption sample / totalProposals sample)

confirmedProposals :: Trace UBLOCK -> Double
confirmedProposals sample =
  traceStates OldestFirst sample
    & fmap upistate
    & fmap Update.confirmedProposals
    & fmap dom
    & fold
    & Set.size
    & fromIntegral

totalProposals :: Trace UBLOCK -> Double
totalProposals sample =
  traceSignals OldestFirst sample
    & fmap optionalUpdateProposal
    & catMaybes
    & fmap Update._upId
    & length
    & fromIntegral

-- Count the number of votes for proposals that were included in the same
-- block as the votes.
numberOfVotesForBlockProposal :: Trace UBLOCK -> Int
numberOfVotesForBlockProposal sample =
  traceSignals OldestFirst sample
    & filter voteForBlockProposal
    & length
  where
    voteForBlockProposal :: UBlock -> Bool
    voteForBlockProposal UBlock {optionalUpdateProposal, votes} =
      case optionalUpdateProposal of
        Nothing ->
          False
        Just updateProposal ->
          Update._upId updateProposal `elem` (Update._vPropId <$> votes)

numberOfBlocksWithoutVotes :: Trace UBLOCK -> Int
numberOfBlocksWithoutVotes sample =
  traceSignals OldestFirst sample
    & filter (null . votes)
    & length

numberOfBlocksWithoutUpdateProposals :: Trace UBLOCK -> Int
numberOfBlocksWithoutUpdateProposals sample =
  traceSignals OldestFirst sample
    & filter (isNothing . optionalUpdateProposal)
    & length

proposalsScheduledForAdoption :: Trace UBLOCK -> Double
proposalsScheduledForAdoption sample =
  traceStates OldestFirst sample
    & fmap upistate
    -- We concat all the future adoptions together
    & concatMap Update.futureAdoptions
    -- Get the protocol versions of the future adoptions
    & fmap (fst . snd)
    -- Turn the list of versions into a list, since we're
    -- interested in the number of unique protocol-versions
    -- that are scheduled for adoption through the trace
    -- states.
    & Set.fromList
    & Set.size
    & fromIntegral

-- | Sample a 'UBLOCK' trace, and print different metrics. This can be used in the REPL, and it is
-- useful for understanding the traces produced by the 'UBLOCK' transition system.
ublockSampleTraceMetrics :: Word64 -> IO ()
ublockSampleTraceMetrics maxTraceSize = do
  sample <- randomTraceOfSize @UBLOCK () maxTraceSize
  let (_slot, _dms, k, numberOfGenesisKeys) = _traceEnv sample
  traverse_
    print
    [ "k = "
        ++ show k,
      "genesis keys = "
        ++ show numberOfGenesisKeys,
      "trace length = "
        ++ show (traceLength sample),
      "proposals = "
        ++ show (totalProposals sample),
      "confirmed proposals = "
        ++ show (confirmedProposals sample),
      "scheduled proposals = "
        ++ show (proposalsScheduledForAdoption sample),
      "total number of votes = "
        ++ show (numberOfVotes sample),
      "blocks without votes = "
        ++ show (numberOfBlocksWithoutVotes sample),
      "votes for block proposal in same block = "
        ++ show (numberOfVotesForBlockProposal sample),
      "ratio of votes for block proposal in same block = "
        ++ show (fromIntegral (numberOfVotesForBlockProposal sample) / totalProposals sample),
      "proposals scheduled for adoption = "
        ++ show (proposalsScheduledForAdoption sample),
      "confirmed / total proposals = "
        ++ show (confirmedProposals sample / totalProposals sample),
      "scheduled / total proposals = "
        ++ show (proposalsScheduledForAdoption sample / totalProposals sample),
      "scheduled / confirmed proposals = "
        ++ show (proposalsScheduledForAdoption sample / confirmedProposals sample)
    ]

numberOfVotes :: Trace UBLOCK -> Int
numberOfVotes sample =
  traceSignals OldestFirst sample
    & concatMap votes
    & length

--------------------------------------------------------------------------------
-- Invalid trace generation
--------------------------------------------------------------------------------

invalidRegistrationsAreGenerated :: Property
invalidRegistrationsAreGenerated =
  withTests 300 $
    Transition.Generator.invalidSignalsAreGenerated
      @UPIREG
      ()
      [(1, invalidUPropGen)]
      100
      (Update.Test.coverUpiregFailures 2)
  where
    invalidUPropGen :: SignalGenerator UPIREG
    invalidUPropGen env st = do
      uprop <- sigGen @UPIREG env st
      tamperWithUpdateProposal env st uprop

invalidSignalsAreGenerated :: Property
invalidSignalsAreGenerated =
  withTests 300 $
    Transition.Generator.invalidSignalsAreGenerated
      @UBLOCK
      ()
      [(1, invalidUBlockGen)]
      100
      (Update.Test.coverUpivoteFailures 2)
  where
    invalidUBlockGen :: SignalGenerator UBLOCK
    invalidUBlockGen env st = do
      ublock <- sigGen @UBLOCK env st
      Gen.choice
        [ do
            uprop <- sigGen @UPIREG (upienv st) (upistate st)
            tamperedUprop <- tamperWithUpdateProposal (upienv st) (upistate st) uprop
            pure $! ublock {optionalUpdateProposal = Just tamperedUprop},
          do
            tamperedVotes <- tamperWithVotes (upienv st) (upistate st) (votes ublock)
            pure $! ublock {votes = tamperedVotes}
        ]
