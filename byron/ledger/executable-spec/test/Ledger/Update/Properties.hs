{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Ledger.Update.Properties
  ( upiregTracesAreClassified
  , upiregRelevantTracesAreCovered
  ) where

import Data.Function ((&))
import qualified Data.Map as Map
import Hedgehog (Property, cover, forAll, property, success, withTests)

import Control.State.Transition.Generator (trace, classifyTraceLength)
import Control.State.Transition.Trace
  ( Trace
  , TraceOrder(OldestFirst)
  , traceLength
  , _traceInitState
  , traceSignals
  )

import Ledger.Update (UPIREG, UProp, _maxBkSz, PParams, protocolParameters)
import qualified Ledger.Update as Update

-- TODO: factor out duplication. Put this in Transition.Generator module!
upiregTracesAreClassified :: Property
upiregTracesAreClassified = property $ do
  let (tl, step) = (500, 50)
  tr <- forAll (trace @UPIREG tl)
  classifyTraceLength tr tl step
  success

upiregRelevantTracesAreCovered :: Property
upiregRelevantTracesAreCovered = withTests 200 $ property $ do
  sample <- forAll (trace @UPIREG 200)

  -- TODO:  increase the major version.
  cover 40
    "at least 30% of the update proposals increase the major version"
    (0.25 <= ratio increaseMajor sample)

  -- TODO: increase the minor version
  cover 40
    "at least 30% of the update proposals increase the minor version"
    (0.25 <= ratio increaseMinor sample)

  -- TODO does not update the pv
  cover 40
    "at least 50% of the update proposals do not change the protocol version"
    (0.30 <= ratio dontChangeProtocolVersion sample)

  -- TODO: OPTIONAL: we have an uniform distribution of issuers
  --
  -- Here we could simply compute how many up's a given issuer should have made
  -- if everybody made the same number of proposals, and check that each issuer
  -- didn't produce less than, say 50% of that.

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease max block size
  cover 50
    "at least 20% of the update proposals decrease the maximum block size"
    (0.2 <= ratio (wrtCurrentProtocolParameters _maxBkSz Decreases) sample)

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease max header size

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease max transaction size

  -- TODO: OPTIONAL: proportion of update proposals that increase/decrease ... all the rest ...

  -- And this might get boring soon ...

  -- TODO does not change the software version
  -- PROBLEM! In the initial state @avs@ is empty, so we will never be able to let the application version unchanged!
  -- So we need to remove this!
  -- cover 10
  --   "at least ... % of the update proposals do not change the software version"
  --   (0.1 <= ratio dontChangeSoftwareVersion sample)

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

    dontChangeSoftwareVersion :: Trace UPIREG -> Int
    dontChangeSoftwareVersion traceSample
      = fmap Update._upSwVer (traceSignals OldestFirst traceSample)
      -- TODO: abstract `maybe False ((Update._svVer sv ==) . fst3) $ Map.lookup (Update._svName sv) avs` pattern awayy!
      & filter (\sv -> maybe False ((Update._svVer sv ==) . fst3) $ Map.lookup (Update._svName sv) avs)
      & length
      where
        fst3 (x, _, _) = x
        avs = Update.avs . _traceInitState $ traceSample

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


data Change = Increases | Decreases | RemainsTheSame
