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

import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe)
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
 )
import Cardano.Ledger.Conway.Rules (EnactState)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), UtxoEnv (..))
import Cardano.Ledger.State (EraCertState (..))
import Control.State.Transition.Extended (TRC (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Utils (runSTS)
import qualified Data.Text as T

data ConwayLedgerExecContext era
  = ConwayLedgerExecContext
  { clecPolicyHash :: StrictMaybe ScriptHash
  , clecEnactState :: EnactState era
  , clecUtxoExecContext :: UtxoExecContext era
  }
  deriving (Generic)

instance Inject (ConwayLedgerExecContext era) (StrictMaybe ScriptHash) where
  inject = clecPolicyHash

instance Inject (ConwayLedgerExecContext ConwayEra) (EnactState ConwayEra) where
  inject = clecEnactState

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
  , ToExpr (Tx era)
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  , ToExpr (TxAuxData era)
  , ToExpr (PParamsHKD Identity era)
  , EraCertState era
  , ToExpr (CertState era)
  ) =>
  ToExpr (ConwayLedgerExecContext era)

instance EraPParams era => EncCBOR (ConwayLedgerExecContext era) where
  encCBOR ConwayLedgerExecContext {..} =
    encode $
      Rec ConwayLedgerExecContext
        !> To clecPolicyHash
        !> To clecEnactState

instance ExecSpecRule "LEDGER" ConwayEra where
  type ExecContext "LEDGER" ConwayEra = ConwayLedgerExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction Agda.ledgerStep

  extraInfo globals ConwayLedgerExecContext {..} (TRC (LedgerEnv {..}, LedgerState {..}, sig)) _ =
    extraInfo @"UTXOW" @ConwayEra
      globals
      clecUtxoExecContext
      (TRC (utxoEnv, lsUTxOState, sig))
      stFinal
    where
      utxoEnv = UtxoEnv ledgerSlotNo ledgerPp lsCertState
      stFinal =
        first (T.pack . show) $
          runSTS @"UTXOW" @ConwayEra globals utxoEnv lsUTxOState sig
