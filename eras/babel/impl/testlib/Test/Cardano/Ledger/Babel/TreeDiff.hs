{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babel.TreeDiff (
  module Test.Cardano.Ledger.Babbage.TreeDiff,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (ContextError)
import Cardano.Ledger.Babel (BabelEra)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Rules
import Cardano.Ledger.Babel.Scripts
import Cardano.Ledger.Babel.TxBody
import Cardano.Ledger.Babel.TxCert
import Cardano.Ledger.Babel.TxInfo (BabelContextError)
import Cardano.Ledger.BaseTypes
import Control.State.Transition.Extended (STS (..))
import Test.Cardano.Data.TreeDiff ()
import Test.Cardano.Ledger.Babbage.TreeDiff
import Test.Cardano.Ledger.Conway.TreeDiff ()

-- Scripts
instance ToExpr (PlutusScript (BabelEra c))

instance ToExpr (BabelPlutusPurpose AsIx era)

instance
  ( Era era
  , ToExpr (TxCert era)
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (BabelPlutusPurpose AsItem era)

-- PlutusContext
instance
  ( Era era
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (TxCert era)
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  ) =>
  ToExpr (BabelContextError era)

-- TxCert

instance ToExpr (BabelDelegCert c)

instance ToExpr (BabelGovCert c)

-- Rules
instance
  ( ToExpr (PlutusPurpose AsItem era)
  , ToExpr (ContextError era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (BabelUtxosPredFailure era)

-- TxBody
instance
  (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (TxOut era)) =>
  ToExpr (BabelTxBodyRaw era)

instance
  (EraPParams era, ToExpr (PParamsHKD StrictMaybe era), ToExpr (TxOut era)) =>
  ToExpr (BabelTxBody era)

-- Rules/Ledger
instance
  ( ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , ToExpr (PredicateFailure (EraRule "GOV" era))
  , ToExpr (PredicateFailure (EraRule "CERTS" era))
  ) =>
  ToExpr (BabelLedgerPredFailure era)

instance
  ( ToExpr (Event (EraRule "CERTS" era))
  , ToExpr (Event (EraRule "UTXOW" era))
  , ToExpr (Event (EraRule "GOV" era))
  ) =>
  ToExpr (BabelLedgerEvent era)

instance ToExpr (TxOut era) => ToExpr (BabelUtxosEvent era)

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  ToExpr (BabelUtxoPredFailure era)

instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (BabelUtxowPredFailure era)
