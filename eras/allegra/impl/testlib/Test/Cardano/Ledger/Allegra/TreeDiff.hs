{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.TreeDiff (
  module Test.Cardano.Ledger.Shelley.TreeDiff,
) where

import Cardano.Ledger.Allegra.Rules
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.PParams
import Control.State.Transition.Extended (STS (..))
import Test.Cardano.Ledger.Shelley.TreeDiff

-- Scripts
instance ToExpr ValidityInterval

instance ToExpr (TimelockRaw era)

instance ToExpr (Timelock era)

-- TxAuxData
instance ToExpr (AllegraTxAuxDataRaw era)

instance ToExpr (AllegraTxAuxData era)

-- TxBody
instance
  ( ToExpr ma
  , ToExpr (TxOut era)
  , ToExpr (TxCert era)
  , ToExpr (Update era)
  ) =>
  ToExpr (AllegraTxBodyRaw ma era)

instance
  ( ToExpr (TxOut era)
  , ToExpr (TxCert era)
  , ToExpr (Update era)
  ) =>
  ToExpr (AllegraTxBody era)

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
