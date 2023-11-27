{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.TreeDiff (
  module Test.Cardano.Ledger.Alonzo.TreeDiff,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules
import Cardano.Ledger.Babbage.TxBody
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Compactible
import Cardano.Ledger.Shelley.Rules
import Test.Cardano.Ledger.Alonzo.TreeDiff

-- Core
deriving newtype instance ToExpr CoinPerByte

-- PParams
instance ToExpr (BabbagePParams StrictMaybe era)

instance ToExpr (BabbagePParams Identity era)

-- TxOut
instance
  ( ToExpr (CompactAddr (EraCrypto era))
  , ToExpr (CompactForm (Value era))
  , ToExpr (Script era)
  ) =>
  ToExpr (BabbageTxOut era)

-- TxBody
instance
  (Era era, ToExpr (TxOut era), ToExpr (TxCert era), ToExpr (PParamsUpdate era)) =>
  ToExpr (BabbageTxBodyRaw era)

instance
  (Era era, ToExpr (TxOut era), ToExpr (TxCert era), ToExpr (PParamsUpdate era)) =>
  ToExpr (BabbageTxBody era)

-- Rules/Utxo
instance
  ( ToExpr (AlonzoUtxoPredFailure era)
  , ToExpr (TxOut era)
  ) =>
  ToExpr (BabbageUtxoPredFailure era)

-- Rules/Utxow
instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (TxCert era)
  ) =>
  ToExpr (BabbageUtxowPredFailure era)
