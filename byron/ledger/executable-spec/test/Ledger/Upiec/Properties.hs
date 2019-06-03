{-# LANGUAGE TypeApplications #-}

module Ledger.Upiec.Properties
  ( noProtVerChange
  , protVerChangeAdopt
  , protVerChangeSameComponents
  , protVerChangeEmptyComponents
  ) where

import           Control.Lens (_1, _2, _3, _4, _5, _6, _7, _8, _9, (^.))
import           Control.State.Transition (applySTS, TRC(..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Ledger.Core (Slot)
import           Ledger.Update (UpId)
import qualified Ledger.Update.Generators as UpdateGen
import           Hedgehog
import           Ledger.Update (PVBUMP, UPIEC, ProtVer, PParams)


-- Property #1 for the UPIEC STS
--
-- If PVBUMP transitions into a state with the same protocol version,
-- the UPIEC STS results in the same state it started in.
noProtVerChange :: Property
noProtVerChange = property $ do
  jc <- forAll UpdateGen.noProtVerChangeJC
  let (_, st, _) = jc
  case applySTS @UPIEC (TRC jc) of
    Right st' -> st === st'
    Left _    -> failure

-- Property #2 for the UPIEC STS
--
-- If PVBUMP transitions into a state with a new different protocol
-- version, the UPIEC STS results in a state that adopts the new
-- protocol version (pv) and new protocol parameters (pps).
protVerChangeAdopt :: Property
protVerChangeAdopt = property $ do
  jc <- forAll UpdateGen.protVerChangeJC
  let
    (s_n, st, _) = jc
    (pv, pps)    = st ^. _1 :: (ProtVer, PParams)
    fads         = st ^. _2 :: [(Slot, (ProtVer, PParams))]
  case applySTS @PVBUMP (TRC ((s_n, fads), (pv, pps), ())) of
    Left _            -> failure
    Right (pv', pps') -> do
      case applySTS @UPIEC (TRC jc) of
        Right st' -> do
          let (pvst', ppsst') = st' ^. _1 :: (ProtVer, PParams)
          pv   /== pvst'
          pv'  === pvst'
          pps' === ppsst'
        Left _    -> failure

-- Property #3 for the UPIEC STS
--
-- If PVBUMP transitions into a state with a new different protocol
-- version, the application versions (avs) and registered software
-- update proposals (raus) components of UPIEC's state stay the same.
protVerChangeSameComponents :: Property
protVerChangeSameComponents = property $ do
  jc <- forAll UpdateGen.protVerChangeJC
  let
    (s_n, st, _) = jc
    (pv, pps)    = st ^. _1 :: (ProtVer, PParams)
    fads         = st ^. _2 :: [(Slot, (ProtVer, PParams))]
  case applySTS @PVBUMP (TRC ((s_n, fads), (pv, pps), ())) of
    Left  _ -> failure
    Right _ -> do
      case applySTS @UPIEC (TRC jc) of
        Right st' -> do
          let
            pvst' = fst (st' ^. _1) :: ProtVer

            avs   = st  ^. _3
            avs'  = st' ^. _3

            raus  = st  ^. _5
            raus' = st' ^. _5

          pv   /== pvst'
          avs  === avs'
          raus === raus'
        Left _    -> failure

-- Property #4 for the UPIEC STS
--
-- If PVBUMP transitions into a state with a new different protocol
-- version, the following components of UPIEC's state are set to an
-- empty set or map (according to the component's type):
--
--   future protocol version adoptions (fads)
--   registered protocol update proposals (rpus)
--   confirmed proposals (cps)
--   proposal votes (vts)
--   endorsement-key pairs (bvs)
--   proposal timestamps (pws)
protVerChangeEmptyComponents :: Property
protVerChangeEmptyComponents = property $ do
  jc <- forAll UpdateGen.protVerChangeJC
  let
    (s_n, st, _) = jc
    (pv, pps)    = st ^. _1 :: (ProtVer, PParams)
    fads         = st ^. _2 :: [(Slot, (ProtVer, PParams))]
  case applySTS @PVBUMP (TRC ((s_n, fads), (pv, pps), ())) of
    Left  _ -> failure
    Right _ -> do
      case applySTS @UPIEC (TRC jc) of
        Right st' -> do
          let
            pvst' = fst (st' ^. _1) :: ProtVer
            fads' = st' ^. _2 :: [(Slot, (ProtVer, PParams))]
            rpus' = st' ^. _4
            cps'  = st' ^. _6
            vts'  = st' ^. _7
            bvs'  = st' ^. _8
            pws'  = st' ^. _9 :: Map UpId Slot

          pv /== pvst'
          assert $     null fads'
          assert $ Map.null rpus'
          assert $ Map.null cps'
          assert $ Set.null vts'
          assert $ Set.null bvs'
          assert $ Map.null pws'
        Left _    -> failure
