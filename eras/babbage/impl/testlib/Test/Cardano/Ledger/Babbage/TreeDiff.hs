{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 914
import Cardano.Ledger.Address
#endif
import Cardano.Ledger.Alonzo.Rules
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.PParams
import Cardano.Ledger.Babbage.Rules
import Cardano.Ledger.Babbage.TxBody
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Compactible
import Cardano.Ledger.Shelley.Rules
import Test.Cardano.Ledger.Alonzo.TreeDiff

-- Core
deriving newtype instance ToExpr CoinPerByte

-- Scripts
instance ToExpr (PlutusScript BabbageEra)

-- PlutusContext
instance ToExpr (PlutusPurpose AsIx era) => ToExpr (BabbageContextError era)

-- PParams
instance ToExpr (BabbagePParams StrictMaybe era)

instance ToExpr (BabbagePParams Identity era)

-- TxOut
instance
  ( ToExpr (CompactForm (Value era))
  , ToExpr (Script era)
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  , ToExpr CompactAddr
#endif
  ) =>
  ToExpr (BabbageTxOut era)

-- TxBody
instance ToExpr BabbageTxBodyRaw

instance ToExpr (TxBody BabbageEra)

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
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (BabbageUtxowPredFailure era)

instance ToExpr (Tx BabbageEra)
