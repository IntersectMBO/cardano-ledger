{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.TreeDiff (
  module Test.Cardano.Ledger.Mary.TreeDiff,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.Plutus.Evaluate
import Cardano.Ledger.Alonzo.Plutus.TxInfo
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxAuxData
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.Alonzo.UTxO
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Compactible
import Cardano.Ledger.Plutus.Evaluate (PlutusWithContext (..))
import Cardano.Ledger.Shelley.Rules
import qualified Data.TreeDiff.OMap as OMap
import PlutusLedgerApi.Common (EvaluationError (..), ExBudget, ExCPU, ExMemory, SatInt)
import Test.Cardano.Ledger.Mary.TreeDiff

-- Scripts
instance ToExpr (PlutusScript AlonzoEra)

instance (ToExpr (PlutusScript era), ToExpr (NativeScript era)) => ToExpr (AlonzoScript era)

instance ToExpr (AlonzoPlutusPurpose AsIx era)

instance ToExpr (TxCert era) => ToExpr (AlonzoPlutusPurpose AsItem era)

deriving newtype instance ToExpr ix => ToExpr (AsIx ix it)

deriving newtype instance ToExpr it => ToExpr (AsItem ix it)

instance (ToExpr ix, ToExpr it) => ToExpr (AsIxItem ix it)

instance ToExpr (PlutusPurpose AsIxItem era) => ToExpr (AlonzoScriptsNeeded era)

instance ToExpr (TxCert era) => ToExpr (AlonzoPlutusPurpose AsIxItem era)

-- Core
deriving newtype instance ToExpr CoinPerWord

-- TxAuxData
instance ToExpr (NativeScript era) => ToExpr (AlonzoTxAuxDataRaw era)

instance ToExpr (NativeScript era) => ToExpr (AlonzoTxAuxData era)

-- PParams
deriving newtype instance ToExpr OrdExUnits

instance ToExpr (AlonzoPParams StrictMaybe era)

instance ToExpr (AlonzoPParams Identity era)

-- TxWits
instance ToExpr (PlutusPurpose AsIx era) => ToExpr (RedeemersRaw era)

instance ToExpr (PlutusPurpose AsIx era) => ToExpr (Redeemers era)

instance
  ( Era era
  , ToExpr (TxDats era)
  , ToExpr (Redeemers era)
  , ToExpr (Script era)
  ) =>
  ToExpr (AlonzoTxWitsRaw era)

instance
  ( Era era
  , ToExpr (TxDats era)
  , ToExpr (Redeemers era)
  , ToExpr (Script era)
  ) =>
  ToExpr (AlonzoTxWits era)

instance ToExpr (Data era) => ToExpr (TxDatsRaw era)

instance ToExpr (Data era) => ToExpr (TxDats era)

-- TxOut
instance ToExpr Addr28Extra

instance ToExpr DataHash32

instance ToExpr (CompactForm (Value era)) => ToExpr (AlonzoTxOut era)

-- TxBody
instance ToExpr (AlonzoTxBodyRaw TopTx AlonzoEra) where
  toExpr AlonzoTxBodyRaw {..} =
    Rec "AlonzoTxBodyRaw" $
      OMap.fromList
        [ ("atbrInputs", toExpr atbrInputs)
        , ("atbrCollateral", toExpr atbrCollateral)
        , ("atbrOutputs", toExpr atbrOutputs)
        , ("atbrCerts", toExpr atbrCerts)
        , ("atbrWithdrawals", toExpr atbrWithdrawals)
        , ("atbrTxFee", toExpr atbrTxFee)
        , ("atbrValidityInterval", toExpr atbrValidityInterval)
        , ("atbrUpdate", toExpr atbrUpdate)
        , ("atbrReqSignerHashes", toExpr atbrReqSignerHashes)
        , ("atbrMint", toExpr atbrMint)
        , ("atbrScriptIntegrityHash", toExpr atbrScriptIntegrityHash)
        , ("atbrAuxDataHash", toExpr atbrAuxDataHash)
        , ("atbrTxNetworkId", toExpr atbrTxNetworkId)
        ]

instance ToExpr (TxBody TopTx AlonzoEra)

-- Tx
instance ToExpr IsValid

instance
  (ToExpr (TxBody TopTx era), ToExpr (TxWits era), ToExpr (TxAuxData era)) =>
  ToExpr (AlonzoTx TopTx era)
  where
  toExpr AlonzoTx {..} =
    Rec "AlonzoTx" $
      OMap.fromList
        [ ("atBody", toExpr atBody)
        , ("atWits", toExpr atWits)
        , ("atIsValid", toExpr atIsValid)
        , ("atAuxData", toExpr atAuxData)
        ]

-- Plutus/TxInfo
instance ToExpr (AlonzoContextError era)

instance
  ( ToExpr (ContextError era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (CollectError era)

-- Rules/Utxo
instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  ToExpr (AlonzoUtxoPredFailure era)

-- Rules/Utxos
instance ToExpr FailureDescription

instance ToExpr TagMismatchDescription

instance
  ( ToExpr (PlutusPurpose AsItem era)
  , ToExpr (EraRuleFailure "PPUP" era)
  , ToExpr (ContextError era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (AlonzoUtxosPredFailure era)

-- Rules/Utxow
instance
  ( Era era
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (TxCert era)
  ) =>
  ToExpr (AlonzoUtxowPredFailure era)

instance ToExpr (Event (EraRule "UTXO" era)) => ToExpr (AlonzoUtxowEvent era)

instance ToExpr (Event (EraRule "UTXOS" era)) => ToExpr (AlonzoUtxoEvent era)

instance
  ( ToExpr (EraRuleEvent "PPUP" era)
  , ToExpr (TxOut era)
  , ToExpr PlutusWithContext
  ) =>
  ToExpr (AlonzoUtxosEvent era)

instance
  ToExpr ScriptHash =>
  ToExpr PlutusWithContext
  where
  toExpr PlutusWithContext {..} =
    Rec "PlutusWithContext" $
      OMap.fromList
        [ ("pwcProtocolVersion", toExpr pwcProtocolVersion)
        , ("pwcScriptHash", toExpr pwcScriptHash)
        , ("pwcExUnits", toExpr pwcExUnits)
        , ("pwcCostModel", toExpr pwcCostModel)
        ]

instance
  ToExpr (PredicateFailure (EraRule "LEDGERS" era)) =>
  ToExpr (AlonzoBbodyPredFailure era)

instance
  ToExpr (Event (EraRule "LEDGERS" era)) =>
  ToExpr (AlonzoBbodyEvent era)

instance ToExpr EvaluationError where
  toExpr = toExpr . show

instance ToExpr SatInt

instance ToExpr ExCPU

instance ToExpr ExMemory

instance ToExpr ExBudget

instance
  ( ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (PlutusScript era)
  , ToExpr (ContextError era)
  ) =>
  ToExpr (TransactionScriptFailure era)

deriving newtype instance ToExpr (Tx TopTx AlonzoEra)
