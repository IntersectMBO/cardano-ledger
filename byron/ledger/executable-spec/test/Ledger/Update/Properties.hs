{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.Update.Properties
  ( upiregTracesAreClassified
  , upiregRelevantTracesAreCovered
  , onlyValidSignalsAreGenerated
  ) where

import qualified Data.Bimap as Bimap
import Data.Function ((&))
import Data.List.Unique (count)
import Numeric.Natural (Natural)
import Hedgehog
  ( Property
  , cover
  , forAll
  , property
  , withTests
  )

import Control.State.Transition.Generator
  ( ratio
  , trace
  , traceLengthsAreClassified
  )
import qualified Control.State.Transition.Generator as TransitionGenerator
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , traceLength
  , _traceInitState
  , traceSignals
  , _traceEnv
  )

import Ledger.Update (UPIREG, PParams, protocolParameters)
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
