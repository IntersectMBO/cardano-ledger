{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.Update.Properties
  ( upiregTracesAreClassified
  , upiregRelevantTracesAreCovered
  , onlyValidSignalsAreGenerated
  ) where

import Control.Monad (void)
import qualified Data.Bimap as Bimap
import Data.Function ((&))
import Data.List.Unique (count)
import Hedgehog
  ( Property
  , cover
  , evalEither
  , forAll
  , property
  , success
  , withTests
  )

import Control.State.Transition
  ( Environment
  , State
  , TRC(TRC)
  , applySTS
  )
import Control.State.Transition.Generator (trace, classifyTraceLength, sigGen)
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , traceLength
  , lastState
  , _traceInitState
  , traceSignals
  , _traceEnv
  )

import Ledger.Update (UPIREG, PParams, protocolParameters)
import qualified Ledger.Update as Update

-- TODO: factor out duplication. Put this in Transition.Generator module!
upiregTracesAreClassified :: Property
upiregTracesAreClassified = withTests 100 $ property $ do
  let (tl, step) = (500, 50)
  tr <- forAll (trace @UPIREG tl)
  classifyTraceLength tr tl step
  success

upiregRelevantTracesAreCovered :: Property
upiregRelevantTracesAreCovered = withTests 300 $ property $ do
  sample <- forAll (trace @UPIREG 400)

  -- TODO:  increase the major version.
  cover 40
    "at least 30% of the update proposals increase the major version"
    (0.25 <= ratio increaseMajor sample)

  -- TODO: increase the minor version
  cover 40
    "at least 30% of the update proposals increase the minor version"
    (0.25 <= ratio increaseMinor sample)

  -- TODO does not update the pv
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
  -- proposals per genesis key. For this example, in this test we would check
  -- that in 50% of the generated traces, the deviation from the fair
  -- distribution (25 update proposals per-key) is no greater than half its
  -- expected value (25 * 0.5 = 12.5).
  cover 50
    "the distribution of the proposal issuers is roughly fair"
    ( safeMaximum (issuersDeviationsWrtUniformDistribution sample)
      <=
      expectedNumberOfUpdateProposalsPerKey sample * 0.5
    )

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease max block size
  cover 50
    "at least 30% of the update proposals decrease the maximum block size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz Decreases) sample)

  cover 50
    "at least 30% of the update proposals increase the maximum block size"
    (0.3 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz Increases) sample)

  cover 50
    "at least 10% of the update proposals do not change the maximum block size"
    (0.1 <= ratio (wrtCurrentProtocolParameters Update._maxBkSz RemainsTheSame) sample)

  -- TODO: we might need to change 1 to the minimum allowed protocol value.
  cover 20
    "at least 5% of the update proposals set the maximum block size to 1"
    (0.05 <= ratio (Update._maxBkSz `isSetTo` 1) sample)

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease max header size

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease max transaction size

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease ... all the rest ...

  -- And this might get boring soon ...

  where
    -- TODO: factor out this duplication once 570 is merged
    ratio :: Integral a
          => (Trace UPIREG -> a)
          -> Trace UPIREG
          -> Double
    ratio f tr = fromIntegral (f tr) / fromIntegral (traceLength tr)

    increaseMajor :: Trace UPIREG -> Int
    increaseMajor traceSample
      = protocolVersions traceSample
      & fmap (Update._pvMaj)
      & filter (currPvMaj traceSample  <)
      & length

    protocolVersions :: Trace UPIREG -> [Update.ProtVer]
    protocolVersions tr
      = fmap Update._upPV (traceSignals OldestFirst tr)

    -- We can take the current protocol version from the initial state of
    -- UPIREG, since this transition does not change the protocol version.
    currPvMaj = Update._pvMaj . Update.protocolVersion . _traceInitState

    currentProtocolVersion = Update.protocolVersion . _traceInitState

    increaseMinor :: Trace UPIREG -> Int
    increaseMinor traceSample
      = protocolVersions traceSample
      & fmap (Update._pvMin)
      & filter (currPvMaj traceSample  <)
      & length

    dontChangeProtocolVersion :: Trace UPIREG -> Int
    dontChangeProtocolVersion traceSample
      = protocolVersions traceSample
      & filter (currentProtocolVersion traceSample ==)
      & length

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
      = fmap (Update._upIssuer) (traceSignals OldestFirst traceSample)
      & count
      -- Take the list of (issuer, count) pairs and keep the count only.
      & fmap snd
      & fmap deviationFromExpectation
      where
        deviationFromExpectation :: Int -> Double
        deviationFromExpectation numberOfProposalsPerKey =
          abs (fromIntegral numberOfProposalsPerKey - expectedNumberOfUpdateProposalsPerKey traceSample)

    delegationMap (_, dms, _) = dms

    safeMaximum xs = if null xs then 0 else maximum xs

data Change = Increases | Decreases | RemainsTheSame

-- The generated signals are valid.
--
-- TODO: this tests can be abstracted over any STS that can produce traces.
onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated = withTests 300 $ property $ do
  tr <- forAll (trace @UPIREG 100)
  let
    env :: Environment UPIREG
    env = _traceEnv tr

    st' :: State UPIREG
    st' = lastState tr
  sig <- forAll (sigGen @UPIREG env st')
  void $ evalEither $ applySTS @UPIREG (TRC(env, st', sig))
