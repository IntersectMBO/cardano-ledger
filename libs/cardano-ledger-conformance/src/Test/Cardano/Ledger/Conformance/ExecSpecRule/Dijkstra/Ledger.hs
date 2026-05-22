{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Dijkstra.Ledger (DijkstraLedgerExecContext (..)) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Alonzo.Scripts (AsItem)
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx)
import Cardano.Ledger.BaseTypes (Globals (networkId), Network, StrictMaybe)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Conway.Core (
  EraPParams (..),
  EraTx (..),
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  PlutusPurpose,
  ScriptHash,
  SubTx,
  TxCert,
  TxLevel (..),
 )
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Tx (DijkstraStAnnTx (..))
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.State (ScriptsNeeded, ScriptsProvided)
import Control.DeepSeq (deepseq, rnf)
import Control.State.Transition.Extended (TRC (..))
import Data.Functor.Identity (Identity)
import qualified Data.TreeDiff.OMap as OMap
import GHC.Generics (Generic)
import Lens.Micro
import qualified MAlonzo.Code.Ledger.Dijkstra.Foreign.API as Agda
import qualified Test.Cardano.Ledger.Binary.TreeDiff as TD
import Test.Cardano.Ledger.Common (NFData, ToExpr (..))
import Test.Cardano.Ledger.Conformance (withSpecTransM)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core (
  ExecSpecRule (..),
  ExecSpecTopLevelRule (..),
  SpecTRC (..),
  runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Base (
  SpecTranslate (..),
  askSpecTransM,
  withCtxSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Dijkstra ()
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))
import Test.Cardano.Ledger.Dijkstra.Arbitrary ()
import Test.Cardano.Ledger.Dijkstra.ImpTest ()

data DijkstraLedgerExecContext era
  = DijkstraLedgerExecContext
  { dlecGuardrailsScriptHash :: StrictMaybe ScriptHash
  , dlecEnactState :: Conway.EnactState era
  , dlecUtxoExecContext :: UtxoExecContext era
  , dlecNetworkId :: Network
  }
  deriving (Generic)

instance
  ( EraPParams era
  , EraTx era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  , EraCertState era
  ) =>
  NFData (DijkstraLedgerExecContext era)

instance
  ( EraTx era
  , ToExpr (Tx TopTx era)
  , ToExpr (TxOut era)
  , ToExpr (TxBody TopTx era)
  , ToExpr (TxWits era)
  , ToExpr (TxAuxData era)
  , ToExpr (PParamsHKD Identity era)
  , EraCertState era
  , ToExpr (CertState era)
  ) =>
  ToExpr (DijkstraLedgerExecContext era)

instance
  ( EraPParams era
  , EraCertState era
  , EncCBOR (TxOut era)
  , EncCBOR (Tx TopTx era)
  ) =>
  EncCBOR (DijkstraLedgerExecContext era)
  where
  encCBOR DijkstraLedgerExecContext {..} =
    encode $
      Rec DijkstraLedgerExecContext
        !> To dlecGuardrailsScriptHash
        !> To dlecEnactState
        !> To dlecUtxoExecContext
        !> To dlecNetworkId

-- | Note: 'TxInfoResult' is forced only to WHNF since it contains thunks/functions
-- (via 'PlutusTxInfoResult') that cannot be fully evaluated.
instance
  ( AlonzoEraTx era
  , NFData (Tx TopTx era)
  , NFData (Tx SubTx era)
  , NFData (ScriptsNeeded era)
  , NFData (ScriptsProvided era)
  , NFData (ContextError era)
  ) =>
  NFData (DijkstraStAnnTx TopTx era)
  where
  rnf stAnnTx@(DijkstraStAnnTopTx _ _ _ _ _ _ _) =
    let DijkstraStAnnTopTx {..} = stAnnTx
     in dsattTx `deepseq`
          dsattScriptsNeeded `deepseq`
            dsattScriptsProvided `deepseq`
              dsattPlutusLegacyMode `deepseq`
                dsattPlutusLanguagesUsed `deepseq`
                  dsattPlutusScriptsWithContext `deepseq`
                    rnf dsattSubTransactions

instance
  ( AlonzoEraTx era
  , NFData (Tx SubTx era)
  , NFData (ScriptsNeeded era)
  , NFData (ScriptsProvided era)
  , NFData (ContextError era)
  ) =>
  NFData (DijkstraStAnnTx SubTx era)
  where
  rnf stAnnTx@(DijkstraStAnnSubTx _ _ _ _ _ _) =
    let DijkstraStAnnSubTx {..} = stAnnTx
     in dsastTx `deepseq`
          dsastScriptsNeeded `deepseq`
            dsastScriptsProvided `deepseq`
              dsastTxInfoResult `seq`
                dsastPlutusLanguagesUsed `deepseq`
                  rnf dsastPlutusScriptsWithContext

instance EncCBOR (Tx TopTx era) => EncCBOR (DijkstraStAnnTx TopTx era) where
  encCBOR DijkstraStAnnTopTx {dsattTx} = encCBOR dsattTx

instance
  ( ToExpr (Tx TopTx era)
  , ToExpr (Tx SubTx era)
  , ToExpr (ScriptsNeeded era)
  , ToExpr (ScriptsProvided era)
  , ToExpr (ContextError era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (DijkstraStAnnTx TopTx era)
  where
  toExpr stAnnTx@(DijkstraStAnnTopTx _ _ _ _ _ _ _) =
    let DijkstraStAnnTopTx {..} = stAnnTx
     in TD.Rec "DijkstraStAnnTopTx" $
          OMap.fromList
            [ ("dsattTx", toExpr dsattTx)
            , ("dsattScriptsNeeded", toExpr dsattScriptsNeeded)
            , ("dsattScriptsProvided", toExpr dsattScriptsProvided)
            , ("dsattPlutusLegacyMode", toExpr dsattPlutusLegacyMode)
            , ("dsattPlutusLanguagesUsed", toExpr dsattPlutusLanguagesUsed)
            , ("dsattPlutusScriptsWithContext", toExpr dsattPlutusScriptsWithContext)
            , ("dsattSubTransactions", toExpr dsattSubTransactions)
            ]

-- | Note: 'dsastTxInfoResult' renders as a placeholder since 'TxInfoResult'
-- contains functions that cannot be turned into 'Expr'.
instance
  ( ToExpr (Tx SubTx era)
  , ToExpr (ScriptsNeeded era)
  , ToExpr (ScriptsProvided era)
  , ToExpr (ContextError era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (DijkstraStAnnTx SubTx era)
  where
  toExpr stAnnTx@(DijkstraStAnnSubTx _ _ _ _ _ _) =
    let DijkstraStAnnSubTx {..} = stAnnTx
     in TD.Rec "DijkstraStAnnSubTx" $
          OMap.fromList
            [ ("dsastTx", toExpr dsastTx)
            , ("dsastScriptsNeeded", toExpr dsastScriptsNeeded)
            , ("dsastScriptsProvided", toExpr dsastScriptsProvided)
            , ("dsastTxInfoResult", TD.App "<TxInfoResult>" [])
            , ("dsastPlutusLanguagesUsed", toExpr dsastPlutusLanguagesUsed)
            , ("dsastPlutusScriptsWithContext", toExpr dsastPlutusScriptsWithContext)
            ]

instance ExecSpecRule "LEDGER" DijkstraEra where
  type ExecContext "LEDGER" DijkstraEra = DijkstraLedgerExecContext DijkstraEra

  translateInputs (TRC (env, st, sig)) = do
    DijkstraLedgerExecContext {..} <- askSpecTransM
    agdaEnv <- withCtxSpecTransM (dlecGuardrailsScriptHash, dlecEnactState) $ toSpecRep env
    agdaSt <- withCtxSpecTransM dlecNetworkId $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  translateOutput _ = withSpecTransM dlecNetworkId . toSpecRep

  runAgdaRule = runFromAgdaFunction Agda.ledgerStep

instance ExecSpecTopLevelRule "LEDGER" DijkstraEra where
  mkRuleExecContext globals (TRC (env, state, signal)) =
    DijkstraLedgerExecContext
      { dlecGuardrailsScriptHash =
          state ^. lsUTxOStateL . utxosGovStateL . constitutionGovStateL . constitutionGuardrailsScriptHashL
      , dlecEnactState = mkEnactState $ state ^. lsUTxOStateL . utxosGovStateL
      , dlecUtxoExecContext =
          UtxoExecContext
            { uecTx = signal ^. txStAnnTxG
            , uecUTxO = state ^. utxoL
            , uecUtxoEnv =
                Shelley.UtxoEnv
                  { Shelley.ueSlot = env ^. Shelley.ledgerSlotNoL
                  , Shelley.uePParams = state ^. lsUTxOStateL . utxosGovStateL . curPParamsGovStateL
                  , Shelley.ueCertState = state ^. lsCertStateL
                  }
            }
      , dlecNetworkId = networkId globals
      }
