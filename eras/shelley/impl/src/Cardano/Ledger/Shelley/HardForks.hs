{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.HardForks
  ( aggregatedRewards,
    allowMIRTransfer,
    validatePoolRewardAccountNetID,
    allowScriptStakeCredsToEarnRewards,
    missingScriptsSymmetricDifference,
    forgoRewardPrefilter,
    allowOutsideForecastTTL,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import GHC.Records

aggregatedRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
aggregatedRewards pp = pvMajor (getField @"_protocolVersion" pp) > natVersion @2

-- | Starting with protocol version 5, the MIR certs will also be
-- able to transfer funds between the reserves and the treasury.
-- Additionally, the semantics for the pervious functionality will
-- change a bit. Before version 5 redundancies in the instantaneous
-- reward mapping were handled by overriding. Now they are handled
-- by adding the values and allowing for negatives updates, provided
-- the sum for each key remains positive.
allowMIRTransfer ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
allowMIRTransfer pp = pvMajor (getField @"_protocolVersion" pp) > natVersion @4

-- | Starting with protocol version 5, we will validate the network ID
-- for the reward account listed in stake pool registration certificates.
validatePoolRewardAccountNetID ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validatePoolRewardAccountNetID pp = pvMajor (getField @"_protocolVersion" pp) > natVersion @4

-- | Starting with protocol version 5, Stake Credentials bound by scripts
-- will be eligibile for staking rewards.
allowScriptStakeCredsToEarnRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
allowScriptStakeCredsToEarnRewards pp = pvMajor (getField @"_protocolVersion" pp) > natVersion @4

-- | Starting with protocol version 7, the UTXO rule predicate failure
-- MissingScriptWitnessesUTXOW will not be used for extraneous scripts
missingScriptsSymmetricDifference ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
missingScriptsSymmetricDifference pp = pvMajor (getField @"_protocolVersion" pp) > natVersion @6

-- | Starting with protocol version 7, the reward calculation no longer
-- filters out unregistered stake addresses at the moment the calculation begins.
-- See the Shelley Ledger Errata 17.2.
forgoRewardPrefilter ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
forgoRewardPrefilter pp = pvMajor (getField @"_protocolVersion" pp) > natVersion @6

-- | In versions 5 and 6, we allow the ttl field to lie outside the stability
-- window.
allowOutsideForecastTTL ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
allowOutsideForecastTTL pp =
  let mv = pvMajor (getField @"_protocolVersion" pp)
   in mv == natVersion @5 || mv == natVersion @6
