{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.TreeDiff (
  module Test.Cardano.Ledger.Mary.TreeDiff,
) where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Plutus.Evaluate
import Cardano.Ledger.Alonzo.Plutus.TxInfo
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxAuxData
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.Alonzo.TxWits
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Compactible
import Cardano.Ledger.Shelley.Rules
import Test.Cardano.Ledger.Mary.TreeDiff

-- Scripts
instance ToExpr Tag

instance ToExpr (PlutusScript (AlonzoEra c))

instance ToExpr (PlutusScript era) => ToExpr (AlonzoScript era)

-- Core
deriving newtype instance ToExpr CoinPerWord

-- TxAuxData
instance ToExpr (AlonzoTxAuxDataRaw era)

instance ToExpr (AlonzoTxAuxData era)

-- PParams
deriving newtype instance ToExpr OrdExUnits

instance ToExpr (AlonzoPParams StrictMaybe era)

instance ToExpr (AlonzoPParams Identity era)

-- TxWits
instance ToExpr RdmrPtr

instance ToExpr (RedeemersRaw era)

instance ToExpr (Redeemers era)

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
instance
  (Era era, ToExpr (TxOut era), ToExpr (TxCert era), ToExpr (PParamsUpdate era)) =>
  ToExpr (AlonzoTxBodyRaw era)

instance
  (Era era, ToExpr (TxOut era), ToExpr (TxCert era), ToExpr (PParamsUpdate era)) =>
  ToExpr (AlonzoTxBody era)

-- Tx
instance ToExpr IsValid

instance
  (ToExpr (TxBody era), ToExpr (TxWits era), ToExpr (TxAuxData era)) =>
  ToExpr (AlonzoTx era)

instance ToExpr (TxCert era) => ToExpr (ScriptPurpose era)

-- Plutus/TxInfo
instance ToExpr (ContextError (AlonzoEra c))

instance (ToExpr (ContextError era), ToExpr (TxCert era)) => ToExpr (CollectError era)

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
  ( ToExpr (PPUPPredFailure era)
  , ToExpr (ContextError era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (AlonzoUtxosPredFailure era)

-- Rules/Utxow
instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (TxCert era)
  ) =>
  ToExpr (AlonzoUtxowPredFailure era)
