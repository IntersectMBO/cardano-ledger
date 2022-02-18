{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Ledger.Shelley.HardForks
  ( aggregatedRewards,
    allowMIRTransfer,
    validatePoolRewardAccountNetID,
    allowScriptStakeCredsToEarnRewards,
    translateTimeForPlutusScripts,
    missingScriptsSymmetricDifference,
    forgoRewardPrefilter,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import GHC.Records

aggregatedRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
aggregatedRewards pp = pvMajor (getField @"_protocolVersion" pp) > 2

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
allowMIRTransfer pp = pvMajor (getField @"_protocolVersion" pp) > 4

-- | Starting with protocol version 5, we will validate the network ID
-- for the reward account listed in stake pool registration certificates.
validatePoolRewardAccountNetID ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validatePoolRewardAccountNetID pp = pvMajor (getField @"_protocolVersion" pp) > 4

-- | Starting with protocol version 5, Stake Credentials bound by scripts
-- will be eligibile for staking rewards.
allowScriptStakeCredsToEarnRewards ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
allowScriptStakeCredsToEarnRewards pp = pvMajor (getField @"_protocolVersion" pp) > 4

-- | Starting with protocol version 6, we translate slots to time correctly for
-- Plutus scripts.
translateTimeForPlutusScripts ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
translateTimeForPlutusScripts pp = pvMajor (getField @"_protocolVersion" pp) > 5

-- | Starting with protocol version 7, the UTXO rule predicate failure
-- MissingScriptWitnessesUTXOW will not be used for extraneous scripts
missingScriptsSymmetricDifference ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
missingScriptsSymmetricDifference pp = pvMajor (getField @"_protocolVersion" pp) > 6

-- | Starting with protocol version 7, the reward calculation no longer
-- filters out unregistered stake addresses at the moment the calculation begins.
-- See the Shelley Ledger Errata 17.2.
forgoRewardPrefilter ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
forgoRewardPrefilter pp = pvMajor (getField @"_protocolVersion" pp) > 6
