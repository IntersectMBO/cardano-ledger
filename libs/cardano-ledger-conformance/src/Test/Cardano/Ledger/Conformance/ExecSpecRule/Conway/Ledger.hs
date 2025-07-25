{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Cardano.Ledger.State (EraCertState (..))
import Data.Functor.Identity (Identity)
import GHC.Generics (Generic)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Common (NFData, ToExpr)
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..), runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway (UtxoExecContext (..))
import Test.Cardano.Ledger.Conway.Arbitrary ()

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

  --extraInfo
  --  globals
  --  ConwayLedgerExecContext {..}
  --  LedgerEnv {..}
  --  LedgerState {..}
  --  sig
  --  _ =
  --    extraInfo @"UTXOW" @ConwayEra
  --      globals
  --      clecUtxoExecContext
  --      utxoEnv
  --      lsUTxOState
  --      sig
  --      stFinal
  --    where
  --      utxoEnv = UtxoEnv ledgerSlotNo ledgerPp lsCertState
  --      stFinal =
  --        first showOpaqueErrorString $
  --          runSTS @"UTXOW" @ConwayEra globals utxoEnv lsUTxOState sig

  --testConformance ctx env st sig = property $ do
  --  (specEnv, specSt, specSig) <-
  --    impAnn "Translating the inputs" $
  --      translateInputs @"LEDGER" @ConwayEra env st sig ctx
  --  logDoc $ "ctx:\n" <> ansiExpr ctx
  --  logDoc $ "implEnv:\n" <> ansiExpr env
  --  logDoc $ "implSt:\n" <> ansiExpr st
  --  logDoc $ "implSig:\n" <> ansiExpr sig
  --  logDoc $ "specEnv:\n" <> ansiExpr specEnv
  --  logDoc $ "specSt:\n" <> ansiExpr specSt
  --  logDoc $ "specSig:\n" <> ansiExpr specSig
  --  agdaResTest <-
  --    fmap (second fixup) $
  --      impAnn "Deep evaluating Agda output" $
  --        evaluateDeep $
  --          runAgdaRule @"LEDGER" @ConwayEra specEnv specSt specSig
  --  -- TODO figure out why assertions are failing and then we can remove this
  --  -- whole method
  --  implRes <- tryRunImpRuleNoAssertions @"LEDGER" @ConwayEra (inject env) (inject st) (inject sig)
  --  implResTest <-
  --    impAnn "Translating implementation values to SpecRep" $
  --      expectRightExpr $
  --        runSpecTransM ctx $
  --          bimapM
  --            (fmap showOpaqueErrorString . traverse toTestRep)
  --            (toTestRep . inject @_ @(ExecState "LEDGER" ConwayEra) . fst)
  --            implRes
  --  globals <- use impGlobalsL
  --  let extra =
  --        extraInfo @"LEDGER" @ConwayEra
  --          globals
  --          ctx
  --          (inject env)
  --          (inject st)
  --          (inject sig)
  --          (first showOpaqueErrorString implRes)
  --  logDoc extra
  --  checkConformance @"LEDGER" @ConwayEra
  --    ctx
  --    (inject env)
  --    (inject st)
  --    (inject sig)
  --    implResTest
  --    agdaResTest
