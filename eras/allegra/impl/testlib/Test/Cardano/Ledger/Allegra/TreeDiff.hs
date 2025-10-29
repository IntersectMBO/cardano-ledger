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

module Test.Cardano.Ledger.Allegra.TreeDiff (
  module Test.Cardano.Ledger.Shelley.TreeDiff,
) where

import Cardano.Ledger.Allegra (AllegraEra, Tx (..))
import Cardano.Ledger.Allegra.Rules
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.PParams
import Control.State.Transition.Extended (STS (..))
import qualified Data.TreeDiff.OMap as OMap
import Test.Cardano.Ledger.Shelley.TreeDiff

-- Scripts
instance ToExpr ValidityInterval

instance ToExpr (TimelockRaw era)

instance ToExpr (Timelock era)

-- TxAuxData
instance ToExpr (NativeScript era) => ToExpr (AllegraTxAuxDataRaw era)

instance ToExpr (NativeScript era) => ToExpr (AllegraTxAuxData era)

-- TxBody
instance
  ( ToExpr ma
  , ToExpr (TxOut era)
  , ToExpr (TxCert era)
  , ToExpr (Update era)
  ) =>
  ToExpr (AllegraTxBodyRaw ma TopTx era)
  where
  toExpr AllegraTxBodyRaw {..} =
    Rec
      "AllegraTxBodyRaw"
      $ OMap.fromList
        [ ("atbrInputs", toExpr atbrInputs)
        , ("atbrOutputs", toExpr atbrOutputs)
        , ("atbrCerts", toExpr atbrCerts)
        , ("atbrWithdrawals", toExpr atbrWithdrawals)
        , ("atbrFee", toExpr atbrFee)
        , ("atbrValidityInterval", toExpr atbrValidityInterval)
        , ("atbrUpdate", toExpr atbrUpdate)
        , ("atbrAuxDataHash", toExpr atbrAuxDataHash)
        , ("atbrMint", toExpr atbrMint)
        ]

instance ToExpr (TxBody TopTx AllegraEra)

-- Rules/Utxo
instance
  ( ToExpr (TxOut era)
  , ToExpr (Value era)
  , ToExpr (EraRuleFailure "PPUP" era)
  ) =>
  ToExpr (AllegraUtxoPredFailure era)

instance
  ( Era era
  , ToExpr (TxOut era)
  , ToExpr (Event (EraRule "PPUP" era))
  ) =>
  ToExpr (AllegraUtxoEvent era)

deriving newtype instance ToExpr (Tx TopTx AllegraEra)
