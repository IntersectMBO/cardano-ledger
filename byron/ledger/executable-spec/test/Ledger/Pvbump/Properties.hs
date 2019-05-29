{-# LANGUAGE TypeApplications #-}

module Ledger.Pvbump.Properties where

import           Control.State.Transition (applySTS, TRC(..))
import           Data.Maybe (fromMaybe)
import qualified Ledger.Update.Generators as G
import           Hedgehog
import           Ledger.Core (BlockCount(..), SlotCount(..), minusSlotMaybe)
import           Ledger.GlobalParams (k)
import           Ledger.Update (PVBUMP)


-- Property #1 for the PVBUMP STS
--
-- Given an empty list of mappings from a slot to a pair of a protocol
-- version and protocol parameters (this is the fads part of the
-- environment), the PVBUMP STS results in the same state it started
-- in.
emptyPVUpdate :: Property
emptyPVUpdate = property $ do
  jc <- forAll G.emptyPVUpdateJC
  let (_, st, _) = jc
  case applySTS @PVBUMP (TRC jc) of
    Right st' -> st === st'
    Left _    -> failure

-- Property #2 for the PVBUMP STS
--
-- For s_n <= 2 * k, the resulting state is the same as the state the
-- system started in.
beginningsNoUpdate :: Property
beginningsNoUpdate = property $ do
  jc <- forAll G.beginningsNoUpdateJC
  let (_, st, _) = jc
  case applySTS @PVBUMP (TRC jc) of
    Right st' -> st === st'
    Left _    -> failure

-- Property #3 for the PVBUMP STS
--
-- For s_n > 2 * k, the resulting state is exclusively determined by
-- the last pair from the list comprising a new protocol version and
-- protocol parameters, where the list has at least one pair for a
-- slot s > 2 * k.
lastProposal :: Property
lastProposal = property $ do
  jc <- forAll G.lastProposalJC
  let
    ((s_n, fads), _, _) = jc
    s = fromMaybe
      (error
        "An improper slot generator used! Constraint violated: s_n > 2*k")
      (minusSlotMaybe s_n (SlotCount . (2 *) . unBlockCount $ k))
    expectedSt = snd . last . (filter ((<= s) . fst)) $ fads

  case applySTS @PVBUMP (TRC jc) of
    Right st' -> expectedSt === st'
    Left _    -> failure
