{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Ledger (ConwayLedgerExecContext (..)) where

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core (
  EraPParams (..),
  EraTx (..),
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  ScriptHash,
  TxLevel (..),
 )
import Cardano.Ledger.Conway.Governance
import qualified Cardano.Ledger.Conway.Rules as Conway
import Cardano.Ledger.Shelley.LedgerState
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition.Extended (TRC (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import qualified MAlonzo.Code.Ledger.Conway.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (
  externalFunctions,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow ()
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
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Utils (runSTS)

data ConwayLedgerExecContext era
  = ConwayLedgerExecContext
  { clecGuardrailsScriptHash :: StrictMaybe ScriptHash
  , clecEnactState :: Conway.EnactState era
  , clecUtxoExecContext :: UtxoExecContext era
  }
  deriving (Generic)

instance
  ( EraPParams era
  , EraTx era
  , NFData (TxWits era)
  , NFData (TxAuxData era)
  , EraCertState era
  ) =>
  NFData (ConwayLedgerExecContext era)

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
  ToExpr (ConwayLedgerExecContext era)

instance
  ( EraPParams era
  , EraCertState era
  , EncCBOR (TxOut era)
  , EncCBOR (Tx TopTx era)
  ) =>
  EncCBOR (ConwayLedgerExecContext era)
  where
  encCBOR ConwayLedgerExecContext {..} =
    encode $
      Rec ConwayLedgerExecContext
        !> To clecGuardrailsScriptHash
        !> To clecEnactState
        !> To clecUtxoExecContext

instance ExecSpecRule "LEDGER" ConwayEra where
  type ExecContext "LEDGER" ConwayEra = ConwayLedgerExecContext ConwayEra

  translateInputs (TRC (env, st, sig)) = do
    ConwayLedgerExecContext {..} <- askSpecTransM
    agdaEnv <- withCtxSpecTransM (clecGuardrailsScriptHash, clecEnactState) $ toSpecRep env
    agdaSt <- withCtxSpecTransM () $ toSpecRep st
    agdaSig <- withCtxSpecTransM () $ toSpecRep sig
    pure $ SpecTRC agdaEnv agdaSt agdaSig

  runAgdaRule trc =
    let externalFunctions' = externalFunctions {Agda.extValidPlutusScript = Agda.isValid (strcSignal trc)}
     in runFromAgdaFunction (Agda.ledgerStep externalFunctions') trc

  extraInfo globals ConwayLedgerExecContext {..} (TRC (Shelley.LedgerEnv {..}, LedgerState {..}, sig)) _ =
    extraInfo @"UTXOW" @ConwayEra
      globals
      clecUtxoExecContext
      (TRC (utxoEnv, lsUTxOState, sig))
      stFinal
    where
      utxoEnv = Shelley.UtxoEnv ledgerSlotNo ledgerPp lsCertState
      stFinal =
        first (T.pack . show) $
          runSTS @"UTXOW" @ConwayEra globals utxoEnv lsUTxOState sig

instance ExecSpecTopLevelRule "LEDGER" ConwayEra where
  mkRuleExecContext _ (TRC (env, state, signal)) =
    ConwayLedgerExecContext
      { clecGuardrailsScriptHash =
          state ^. lsUTxOStateL . utxosGovStateL . constitutionGovStateL . constitutionGuardrailsScriptHashL
      , clecEnactState = mkEnactState $ state ^. lsUTxOStateL . utxosGovStateL
      , clecUtxoExecContext =
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
      }
