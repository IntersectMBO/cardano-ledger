{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Ledger (ConwayLedgerExecContext (..)) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity)

import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe)
import Cardano.Ledger.Conway.Core (
  EraPParams (..),
  EraTx,
  EraTxAuxData (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  ScriptHash,
 )
import Cardano.Ledger.Conway.Rules (EnactState)

import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  FixupSpecRep (..),
  checkConformance,
  runSpecTransM,
  showOpaqueErrorString,
  toTestRep,
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway (IsConwayUniv, UtxoExecContext (..), utxoStateSpec)
import Test.Cardano.Ledger.Conway.Arbitrary ()

import Cardano.Ledger.Conway (ConwayEra)
import Constrained (
  Specification (..),
  assert,
  constrained,
  constrained',
  genFromSpec,
  lit,
  satisfies,
  (==.),
 )

import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (Encode (..), encode, (!>))
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..))
import Cardano.Ledger.Shelley.Rules (LedgerEnv (..), UtxoEnv (..))
import Data.Bitraversable (bimapM)
import GHC.Generics (Generic)
import Lens.Micro.Mtl (use)
import qualified Lib as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..), NFData, Testable (..), ToExpr, ansiExpr)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (enactStateSpec)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo (genUtxoExecContext)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow ()
import Test.Cardano.Ledger.Conway.ImpTest (impAnn, impGlobalsL, logDoc, tryRunImpRuleNoAssertions)
import Test.Cardano.Ledger.Imp.Common (expectRightExpr)
import Test.Cardano.Ledger.Shelley.Utils (runSTS)
import UnliftIO (evaluateDeep)

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
  ) =>
  NFData (ConwayLedgerExecContext era)

instance
  ( EraTx era
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxWits era)
  , ToExpr (TxAuxData era)
  , ToExpr (PParamsHKD Identity era)
  ) =>
  ToExpr (ConwayLedgerExecContext era)

instance EraPParams era => EncCBOR (ConwayLedgerExecContext era) where
  encCBOR ConwayLedgerExecContext {..} =
    encode $
      Rec ConwayLedgerExecContext
        !> To clecPolicyHash
        !> To clecEnactState

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "LEDGER" ConwayEra
  where
  type ExecContext fn "LEDGER" ConwayEra = ConwayLedgerExecContext ConwayEra

  genExecContext = do
    ctx <- arbitrary
    env <- genFromSpec @fn TrueSpec
    ConwayLedgerExecContext
      <$> arbitrary
      <*> genFromSpec @fn (enactStateSpec ctx env)
      <*> genUtxoExecContext

  environmentSpec ConwayLedgerExecContext {..} =
    let UtxoExecContext {..} = clecUtxoExecContext
     in constrained' $ \slotNo _ _txIx pp _acntSt _mempool ->
          [ assert $ pp ==. lit (uePParams uecUtxoEnv)
          , assert $ slotNo ==. lit (ueSlot uecUtxoEnv)
          ]

  stateSpec ConwayLedgerExecContext {..} _ =
    let UtxoExecContext {..} = clecUtxoExecContext
     in constrained' $ \utxos certState ->
          [ utxos `satisfies` utxoStateSpec clecUtxoExecContext uecUtxoEnv
          , assert $ certState ==. lit (ueCertState uecUtxoEnv)
          ]

  signalSpec ConwayLedgerExecContext {..} _ _ =
    let UtxoExecContext {..} = clecUtxoExecContext
     in constrained (==. lit uecTx)

  runAgdaRule env st sig = unComputationResult $ Agda.ledgerStep env st sig

  extraInfo
    globals
    ConwayLedgerExecContext {..}
    LedgerEnv {..}
    LedgerState {..}
    sig
    _ =
      extraInfo @fn @"UTXOW" @ConwayEra
        globals
        clecUtxoExecContext
        utxoEnv
        lsUTxOState
        sig
        stFinal
      where
        utxoEnv = UtxoEnv ledgerSlotNo ledgerPp lsCertState
        stFinal =
          first showOpaqueErrorString $
            runSTS @"UTXOW" @ConwayEra globals utxoEnv lsUTxOState sig

  testConformance ctx env st sig = property $ do
    (specEnv, specSt, specSig) <-
      impAnn "Translating the inputs" $
        translateInputs @fn @"LEDGER" @ConwayEra env st sig ctx
    logDoc $ "ctx:\n" <> ansiExpr ctx
    logDoc $ "implEnv:\n" <> ansiExpr env
    logDoc $ "implSt:\n" <> ansiExpr st
    logDoc $ "implSig:\n" <> ansiExpr sig
    logDoc $ "specEnv:\n" <> ansiExpr specEnv
    logDoc $ "specSt:\n" <> ansiExpr specSt
    logDoc $ "specSig:\n" <> ansiExpr specSig
    agdaResTest <-
      fmap (second fixup) $
        impAnn "Deep evaluating Agda output" $
          evaluateDeep $
            runAgdaRule @fn @"LEDGER" @ConwayEra specEnv specSt specSig
    -- TODO figure out why assertions are failing and then we can remove this
    -- whole method
    implRes <- tryRunImpRuleNoAssertions @"LEDGER" @ConwayEra (inject env) (inject st) (inject sig)
    implResTest <-
      impAnn "Translating implementation values to SpecRep" $
        expectRightExpr $
          runSpecTransM ctx $
            bimapM
              (fmap showOpaqueErrorString . traverse toTestRep)
              (toTestRep . inject @_ @(ExecState fn "LEDGER" ConwayEra) . fst)
              implRes
    globals <- use impGlobalsL
    let extra =
          extraInfo @fn @"LEDGER" @ConwayEra
            globals
            ctx
            (inject env)
            (inject st)
            (inject sig)
            (first showOpaqueErrorString implRes)
    logDoc extra
    checkConformance @"LEDGER" @ConwayEra @fn
      ctx
      (inject env)
      (inject st)
      (inject sig)
      implResTest
      agdaResTest
