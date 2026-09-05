{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Era (
  module Test.Cardano.Ledger.Mary.Era,
  AlonzoEraTest,
  mkTestLedgerTxInfo,
) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Plutus.TxInfo
import Cardano.Ledger.Alonzo.UTxO
import Cardano.Ledger.BaseTypes (Inject, ProtVer)
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.State
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Text (Text)
import Data.TreeDiff
import Lens.Micro
import Paths_cardano_ledger_alonzo (getDataFileName)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Alonzo.Examples (
  exampleAlonzoPParams,
  exampleAlonzoPParamsUpdate,
  exampleAlonzoTx,
 )
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common (Arbitrary)
import Test.Cardano.Ledger.Mary.Era
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( MaryEraTest era
  , EraPlutusContext era
  , AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , AlonzoEraUTxO era
  , ToExpr (PlutusScript era)
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsIxItem era)
  , Script era ~ AlonzoScript era
  , EraPlutusTxInfo PlutusV1 era
  , Arbitrary (PlutusPurpose AsIx era)
  , Inject (AlonzoContextError era) (ContextError era)
  ) =>
  AlonzoEraTest era

instance EraTest AlonzoEra where
  type
    EraRulesWithFailures AlonzoEra =
      '[ "BBODY"
       , "DELEG"
       , "DELEGS"
       , "DELPL"
       , "LEDGER"
       , "LEDGERS"
       , "POOL"
       , "PPUP"
       , "UTXO"
       , "UTXOS"
       , "UTXOW"
       ]

  zeroCostModels = zeroTestingCostModels [PlutusV1]

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

  mkEraFullPath = getDataFileName

  exampleTx = exampleAlonzoTx

  examplePParams = exampleAlonzoPParams

  examplePParamsUpdate = exampleAlonzoPParamsUpdate

instance ShelleyEraTest AlonzoEra

instance AllegraEraTest AlonzoEra

instance MaryEraTest AlonzoEra

instance AlonzoEraTest AlonzoEra

-- | This is a construction of `LedgerTxInfo` without any memoization.
mkTestLedgerTxInfo ::
  (EraUTxO era, EraPlutusContext era, ScriptsNeeded era ~ AlonzoScriptsNeeded era) =>
  ProtVer ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx level era ->
  LedgerTxInfo era
mkTestLedgerTxInfo protVer epochInfo systemStart utxo tx =
  let
    scriptsProvided = getScriptsProvided utxo tx
    scriptsNeeded = getScriptsNeeded utxo (tx ^. bodyTxL)
    (_, plutusScriptsUsed) =
      resolveNeededPlutusScriptsWithPurpose protVer scriptsProvided scriptsNeeded mempty
   in
    LedgerTxInfo
      { ltiProtVer = protVer
      , ltiEpochInfo = epochInfo
      , ltiSystemStart = systemStart
      , ltiUTxO = utxo
      , ltiTx = tx
      , ltiScriptsUsed = plutusScriptsUsed
      , ltiScriptHashesUsed = toScriptHashByPurpose plutusScriptsUsed
      , ltiMemoizedSubTransactions = mempty
      }
