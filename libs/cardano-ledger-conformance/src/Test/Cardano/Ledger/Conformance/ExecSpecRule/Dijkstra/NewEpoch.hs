{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Dijkstra.NewEpoch () where

import Cardano.Ledger.BaseTypes (EpochInterval (..), Globals (networkId), Network, addEpochInterval)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Rules (maxKeyAgeEpochs)
import Cardano.Ledger.Shelley.LedgerState (NewEpochState (nesEL))
import Control.State.Transition.Extended (TRC (..))
import GHC.Generics (Generic)
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr (..))
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  ExecSpecTopLevelRule (..),
  SpecTRC (..),
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  unComputationResult_,
  withCtxSpecTransM,
  withSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Dijkstra.ImpTest ()

data DijkstraNewEpochExecContext era
  = DijkstraNewEpochExecContext
  { dneecNetworkId :: Network
  , dneecInputMaxKeyAge :: EpochInterval
  , dneecOutputMaxKeyAge :: EpochInterval
  }
  deriving (Generic)

instance NFData (DijkstraNewEpochExecContext era)

instance ToExpr (DijkstraNewEpochExecContext era)

instance EncCBOR (DijkstraNewEpochExecContext era) where
  encCBOR DijkstraNewEpochExecContext {..} =
    encode $
      Rec DijkstraNewEpochExecContext
        !> To dneecNetworkId
        !> To dneecInputMaxKeyAge
        !> To dneecOutputMaxKeyAge

instance ExecSpecRule "NEWEPOCH" DijkstraEra where
  type ExecContext "NEWEPOCH" DijkstraEra = DijkstraNewEpochExecContext DijkstraEra

  translateInputs (TRC (env, st, sig)) = do
    DijkstraNewEpochExecContext {..} <- askSpecTransM
    agdaEnv <- withCtxSpecTransM () $ toSpecRep env
    agdaSt <- withCtxSpecTransM (dneecNetworkId, dneecInputMaxKeyAge) $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  translateOutput _ =
    withSpecTransM (\DijkstraNewEpochExecContext {..} -> (dneecNetworkId, dneecOutputMaxKeyAge))
      . toSpecRep

  runAgdaRule (SpecTRC env st sig) = unComputationResult_ $ Agda.newEpochStep env st sig

instance ExecSpecTopLevelRule "NEWEPOCH" DijkstraEra where
  mkRuleExecContext globals (TRC (_, st, sig)) =
    DijkstraNewEpochExecContext
      { dneecNetworkId = networkId globals
      , dneecInputMaxKeyAge = maxKeyAgeEpochs globals (nesEL st)
      , dneecOutputMaxKeyAge =
          -- A skipped signal leaves the state unchanged. Do not ask epochInfo
          -- about an arbitrarily distant signal that the rule will ignore.
          if sig == addEpochInterval (nesEL st) (EpochInterval 1)
            then maxKeyAgeEpochs globals sig
            else maxKeyAgeEpochs globals (nesEL st)
      }
