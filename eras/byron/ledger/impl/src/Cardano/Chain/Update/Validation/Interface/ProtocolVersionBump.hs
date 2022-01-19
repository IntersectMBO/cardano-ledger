{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Chain.Update.Validation.Interface.ProtocolVersionBump
  ( Environment (..),
    State (..),
    tryBumpVersion,
  )
where

import Cardano.Chain.Common.BlockCount (BlockCount)
import Cardano.Chain.ProtocolConstants (kUpdateStabilityParam)
import Cardano.Chain.Slotting (SlotNumber, addSlotCount)
import Cardano.Chain.Update.ProtocolParameters (ProtocolParameters)
import Cardano.Chain.Update.ProtocolVersion (ProtocolVersion)
import Cardano.Chain.Update.Validation.Endorsement
  ( CandidateProtocolUpdate (CandidateProtocolUpdate),
    cpuProtocolParameters,
    cpuProtocolVersion,
    cpuSlot,
  )
import Cardano.Prelude hiding (State)

data Environment = Environment
  { k :: !BlockCount,
    epochFirstSlot :: !SlotNumber,
    candidateProtocolVersions :: ![CandidateProtocolUpdate]
  }

data State = State
  { nextProtocolVersion :: !ProtocolVersion,
    nextProtocolParameters :: !ProtocolParameters
  }

-- | Change the protocol version when an epoch change is detected, and there is
-- a candidate protocol update that was confirmed at least @4 * k@ slots before
-- the start of the new epoch, where @k@ is the chain security parameter.
--
-- For a full history of why this is required, see
-- https://github.com/input-output-hk/cardano-ledger-specs/issues/1288
--
-- This corresponds to the @PVBUMP@ rules in the Byron ledger specification.
tryBumpVersion ::
  Environment ->
  State ->
  State
tryBumpVersion env st =
  case stableCandidates of
    (newestStable : _) ->
      let CandidateProtocolUpdate
            { cpuProtocolVersion,
              cpuProtocolParameters
            } = newestStable
       in st
            { nextProtocolVersion = cpuProtocolVersion,
              nextProtocolParameters = cpuProtocolParameters
            }
    _ -> st
  where
    Environment {k, epochFirstSlot, candidateProtocolVersions} = env

    stableCandidates =
      filter ((\x -> addSlotCount (kUpdateStabilityParam k) x <= epochFirstSlot) . cpuSlot) candidateProtocolVersions
