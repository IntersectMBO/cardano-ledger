{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Update.Properties
  ( upiregTracesAreClassified
  , upiregRelevantTracesAreCovered
  , onlyValidSignalsAreGenerated
  , ublockTraceLengthsAreClassified
  , ublockOnlyValidSignalsAreGenerated
  , ublockRelevantTracesAreCovered
  ) where

import GHC.Stack (HasCallStack)

import qualified Data.Bimap as Bimap
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List.Unique (count)
import Data.Maybe (catMaybes, isNothing)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import Hedgehog
  ( Property
  , cover
  , forAll
  , property
  , withTests
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition
  ( Environment
  , PredicateFailure
  , STS
  , Signal
  , State
  , initialRules
  , transitionRules
  , TRC (TRC)
  , IRC (IRC)
  , judgmentContext
  , (?!)
  , trans
  , Embed
  , wrapFailed
  , applySTS
  )
import Control.State.Transition.Generator
  ( ratio
  , trace
  , traceLengthsAreClassified
  , HasTrace
  , sigGen
  , initEnvGen
  )
import qualified Control.State.Transition.Generator as TransitionGenerator
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , traceLength
  , _traceInitState
  , traceSignals
  , _traceEnv
  , traceStates
  )

import Ledger.Core (dom)
import qualified Ledger.Core as Core
import Ledger.GlobalParams (slotsPerEpoch)
import Ledger.Update (UPIREG, UPIVOTES, PParams, protocolParameters, UPIEnv, UPIState, UProp, Vote, emptyUPIState)
import qualified Ledger.Update as Update

upiregTracesAreClassified :: Property
upiregTracesAreClassified =
  withTests 100 $ traceLengthsAreClassified @UPIREG 500 50

upiregRelevantTracesAreCovered :: Property
upiregRelevantTracesAreCovered = withTests 300 $ property $ do
  sample <- forAll (trace @UPIREG 400)

  cover 40
    "at least 30% of the update proposals increase the major version"
    (0.25 <= ratio increaseMajor sample)

  cover 40
    "at least 30% of the update proposals increase the minor version"
    (0.25 <= ratio increaseMinor sample)

  cover 30
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
  cover 50
    "the distribution of the proposal issuers is roughly fair"
    ( safeMaximum (issuersDeviationsWrtUniformDistribution sample)
      <=
      expectedNumberOfUpdateProposalsPerKey sample * 0.5
    )

  --------------------------------------------------------------------------------
  -- Maximum block-size checks
  --------------------------------------------------------------------------------
  cover 50
    "at least 30% of the update proposals decrease the maximum block-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz Decreases) sample)

  cover 50
    "at least 30% of the update proposals increase the maximum block-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz Increases) sample)

  cover 50
    "at least 10% of the update proposals do not change the maximum block-size"
    (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz RemainsTheSame) sample)

  -- TODO: in the future we should change 1 to the minimum allowed protocol
  -- value. But first we need to determine what that value is.
  cover 20
    "at least 5% of the update proposals set the maximum block-size to 1"
    (0.05 <= ratio (Update._maxBkSz `isSetTo` 1) sample)

  --------------------------------------------------------------------------------
  -- Maximum header-size checks
  --------------------------------------------------------------------------------
  cover 50
    "at least 30% of the update proposals decrease the maximum header-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxHdrSz Decreases) sample)

  cover 50
    "at least 30% of the update proposals increase the maximum header-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxHdrSz Increases) sample)

  cover 50
    "at least 10% of the update proposals do not change the maximum header-size"
    (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxHdrSz RemainsTheSame) sample)

  cover 20
    "at least 5% of the update proposals set the maximum header-size to 0"
    (0.05 <= ratio (Update._maxHdrSz `isSetTo` 0) sample)

  --------------------------------------------------------------------------------
  -- Maximum transaction-size checks
  --------------------------------------------------------------------------------
  cover 50
    "at least 30% of the update proposals decrease the maximum transaction-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxTxSz Decreases) sample)

  cover 50
    "at least 30% of the update proposals increase the maximum transaction-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxTxSz Increases) sample)

  cover 50
    "at least 10% of the update proposals do not change the maximum transaction-size"
    (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxTxSz RemainsTheSame) sample)

  cover 20
    "at least 5% of the update proposals set the maximum transaction-size to 0"
    (0.05 <= ratio (Update._maxTxSz `isSetTo` 0) sample)

  --------------------------------------------------------------------------------
  -- Maximum proposal-size checks
  --------------------------------------------------------------------------------
  cover 50
    "at least 30% of the update proposals decrease the maximum proposal-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxPropSz Decreases) sample)

  cover 50
    "at least 30% of the update proposals increase the maximum proposal-size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxPropSz Increases) sample)

  cover 50
    "at least 10% of the update proposals do not change the maximum proposal-size"
    (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxPropSz RemainsTheSame) sample)

  cover 20
    "at least 5% of the update proposals set the maximum proposal-size to 0"
    (0.05 <= ratio (Update._maxPropSz `isSetTo` 0) sample)

  -- NOTE: after empirically determining the checks above are sensible, we can
  -- add more coverage tests for the other protocol parameters.

  where
    increaseMajor :: Trace UPIREG -> Int
    increaseMajor traceSample
      = protocolVersions traceSample
      & fmap Update._pvMaj
      & filter (currPvMaj traceSample  <)
      & length

    protocolVersions :: Trace UPIREG -> [Update.ProtVer]
    protocolVersions tr
      = fmap Update._upPV (traceSignals OldestFirst tr)

    -- We can take the current protocol version from the initial state of
    -- UPIREG, since this transition does not change the protocol version.
    currPvMaj :: Trace UPIREG -> Natural
    currPvMaj = Update._pvMaj . Update.protocolVersion . _traceInitState

    currentProtocolVersion :: Trace UPIREG -> Update.ProtVer
    currentProtocolVersion = Update.protocolVersion . _traceInitState

    increaseMinor :: Trace UPIREG -> Int
    increaseMinor traceSample
      = protocolVersions traceSample
      & fmap Update._pvMin
      & filter (currPvMaj traceSample  <)
      & length

    dontChangeProtocolVersion :: Trace UPIREG -> Int
    dontChangeProtocolVersion traceSample
      = protocolVersions traceSample
      & filter (currentProtocolVersion traceSample ==)
      & length

    -- Given a accessor function for the protocol parameters, count the number
    -- of update proposals that change the parameter value in the way specified
    -- by the 'Change' parameter.
    wrtCurrentProtocolParameters
      :: Ord v
      => (PParams -> v)
      -> Change
      -> Trace UPIREG
      -> Int
    wrtCurrentProtocolParameters parameterValue parameterValueChange traceSample
      = fmap (parameterValue . Update._upParams) (traceSignals OldestFirst traceSample)
      & filter (check parameterValueChange)
      & length
      where
        currentParameterValue = parameterValue . protocolParameters . _traceInitState $ traceSample
        check Increases proposedParameterValue       = currentParameterValue  < proposedParameterValue
        check Decreases proposedParameterValue       = proposedParameterValue < currentParameterValue
        check RemainsTheSame proposedParameterValue  = currentParameterValue == proposedParameterValue

    -- Count the number of times in the sequence of update proposals that the
    -- given protocol value is set to the given value.
    isSetTo
      :: Eq v
      => (PParams -> v)
      -> v
      -> Trace UPIREG
      -> Int
    isSetTo parameterValue value traceSample
      = fmap (parameterValue . Update._upParams) (traceSignals OldestFirst traceSample)
      & filter (value ==)
      & length

    expectedNumberOfUpdateProposalsPerKey :: Trace UPIREG -> Double
    expectedNumberOfUpdateProposalsPerKey traceSample =
      if numberOfGenesisKeys == 0
      then 0
      else fromIntegral $ traceLength traceSample `div` numberOfGenesisKeys
      where
        numberOfGenesisKeys :: Int
        numberOfGenesisKeys = length
                            $ Bimap.keys
                            $ delegationMap (_traceEnv traceSample)

    issuersDeviationsWrtUniformDistribution
      :: Trace UPIREG
      -> [Double]
    issuersDeviationsWrtUniformDistribution traceSample
      = fmap Update._upIssuer (traceSignals OldestFirst traceSample)
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
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @UPIREG 100


-- | Dummy transition system to test blocks with update payload only.
data UBLOCK

-- | An update block
data UBlock
  = UBlock
    { slot :: Core.Slot
    , optionalUpdateProposal :: Maybe UProp
    , votes :: [Vote]
    } deriving (Eq, Show)

-- | Update block state
data UBlockState
  = UBlockState
    { upienv :: UPIEnv
    , upistate :: UPIState
    } deriving (Eq, Show)

instance STS UBLOCK where
  type Environment UBLOCK = UPIEnv

  type State UBLOCK = UBlockState

  type Signal UBLOCK = UBlock

  data PredicateFailure UBLOCK
    = UPIREGFailure (PredicateFailure UPIREG)
    | UPIVOTESFailure (PredicateFailure UPIVOTES)
    | NotIncreasingBlockSlot
    deriving (Eq, Show)

  initialRules
    = [ do
          IRC env <- judgmentContext
          pure UBlockState { upienv = env
                           , upistate = emptyUPIState }
      ]

  transitionRules
    = [ do
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
          pure UBlockState { upienv = (slot ublock, dms, k, ngk )
                           , upistate = upistateAfterVoting }
      ]

instance Embed UPIREG UBLOCK where
  wrapFailed = UPIREGFailure

instance Embed UPIVOTES UBLOCK where
  wrapFailed = UPIVOTESFailure

instance HasTrace UBLOCK where
  initEnvGen = Update.upiEnvGen

  sigGen _env UBlockState {upienv, upistate} =
    Gen.frequency [ (4, generateOnlyVotes)
                  , (1, generateUpdateProposalAndVotes)
                  ]
      where
        generateOnlyVotes =
          UBlock
            <$> nextSlotGen
            <*> pure Nothing
            <*> sigGen @UPIVOTES upienv upistate
        generateUpdateProposalAndVotes = do
          updateProposal <- sigGen @UPIREG upienv upistate
          -- We want to have the possibility of generating votes for the proposal we
          -- registered.
          case applySTS @UPIREG (TRC (upienv, upistate, updateProposal)) of
            Left _ ->
              UBlock
                <$> nextSlotGen
                <*> pure (Just updateProposal)
                <*> sigGen @UPIVOTES upienv upistate
            Right upistateAfterRegistration ->
              UBlock
                <$> nextSlotGen
                <*> pure (Just updateProposal)
                <*> sigGen @UPIVOTES upienv upistateAfterRegistration
        nextSlotGen =
          -- NOTE: in the future, we might want to factor out duplication
          -- w.r.t. @Ledger.Delegation.Properties@ if we find out that no
          -- adaptations to 'nextSlotGen' are needed. For now we duplicate this
          -- here to avoid an early coupling that might be unnecessary.
          incSlot <$> Gen.frequency
                      [ (1, Gen.integral (Range.constant 1 10))
                      , (2, pure $! slotsPerEpoch k + 1)
                      ]
          where
            incSlot c = sn `Core.addSlot` Core.SlotCount c
            (sn, _, k, _) = upienv


ublockTraceLengthsAreClassified :: Property
ublockTraceLengthsAreClassified =
  withTests 100 $ traceLengthsAreClassified @UBLOCK 500 50

ublockOnlyValidSignalsAreGenerated :: HasCallStack => Property
ublockOnlyValidSignalsAreGenerated =
  withTests 300 $ TransitionGenerator.onlyValidSignalsAreGenerated @UBLOCK 100

ublockRelevantTracesAreCovered :: Property
ublockRelevantTracesAreCovered = withTests 300 $ property $ do
  sample <- forAll (trace @UBLOCK 600)

  cover 75
    "at least 50% of the proposals get confirmed"
    (0.50 <= confirmedProposals sample / totalProposals sample)

  cover 20
    "at least 20% of the proposals get unconfirmed"
    (0.20 <= 1 - (confirmedProposals sample / totalProposals sample))

  cover 10
    "at least 2% of the blocks contain votes for proposals issued in the same block "
    (0.02 <= ratio numberOfVotesForBlockProposal sample)

  cover 50
    "at least 30% of blocks contain no votes"
    (0.3 <= ratio numberOfBlocksWithoutVotes sample)

  cover 50
    "at least 10% of blocks contain votes"
    (0.1 <= 1 - ratio numberOfBlocksWithoutVotes sample)

  cover 10
    "at least 20% of blocks contain votes"
    (0.2 <= 1 - ratio numberOfBlocksWithoutVotes sample)

  cover 50
    "at least 20% of the blocks contain no update proposals"
    (0.1 <= ratio numberOfBlocksWithoutUpdateProposals sample)

    where
      confirmedProposals :: Trace UBLOCK -> Double
      confirmedProposals sample = traceStates OldestFirst sample
                                & fmap upistate
                                & fmap Update.confirmedProposals
                                & fmap dom
                                & fold
                                & Set.size
                                & fromIntegral

      totalProposals :: Trace UBLOCK -> Double
      totalProposals sample = traceSignals OldestFirst sample
                            & fmap optionalUpdateProposal
                            & catMaybes
                            & fmap Update._upId
                            & length
                            & fromIntegral

      -- Count the number of votes for proposals that were included in the same
      -- block as the votes.
      numberOfVotesForBlockProposal :: Trace UBLOCK -> Int
      numberOfVotesForBlockProposal sample
        = traceSignals OldestFirst sample
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
      numberOfBlocksWithoutVotes sample
        = traceSignals OldestFirst sample
        & filter (null . votes)
        & length

      numberOfBlocksWithoutUpdateProposals :: Trace UBLOCK -> Int
      numberOfBlocksWithoutUpdateProposals sample
        = traceSignals OldestFirst sample
        & filter (isNothing . optionalUpdateProposal)
        & length
